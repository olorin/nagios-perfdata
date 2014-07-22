-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Nagios.Perfdata.Metric(
    Perfdata(..),
    MetricList,
    Metric(..),
    MetricValue(..),
    HostOrService(..),
    ServicePerfdata(..),
    uomFromString,
    parseReturnCode,
    parseReturnState,
    parseMetricString,
    UOM(..),
    ReturnState(..),
    Threshold(..),
    perfdataServiceDescription,
    metricValueDefault,
    unknownMetricValue,
    isMetricBase,
    convertMetricToBase
) where

import Data.Nagios.Perfdata.Error

import Prelude hiding (takeWhile)
import Data.Int
import qualified Data.ByteString as S
import Control.Monad
import Control.Applicative
import Data.Attoparsec.ByteString.Char8



-- |Value of a performance metric. We may lose some data converting 
-- to doubles here; this may change in the future.
data MetricValue = DoubleValue Double | UnknownValue deriving (Show, Eq)

-- |Value of a min/max/warn/crit threshold, subject to the same 
-- constraints as MetricValue.
data Threshold = DoubleThreshold Double | NoThreshold deriving (Show)

-- |Encapsulates the data in a Nagios performance metric. A service can
-- have several of these.
data Metric = Metric {
    metricValue :: MetricValue,
    metricUOM   :: UOM,
    warnValue :: Threshold,
    critValue :: Threshold,
    minValue :: Threshold,
    maxValue :: Threshold
} deriving (Show)

metricValueDefault :: Metric -> Double -> Double
metricValueDefault Metric{..} d = case metricValue of
    UnknownValue -> d
    DoubleValue x -> x

unknownMetricValue :: Metric -> Bool
unknownMetricValue m = case metricValue m of
    UnknownValue -> True
    _ -> False

-- |List of metrics by metric name.
type MetricList = [(String, Metric)]

-- |Nagios unit of measurement. NullUnit is an empty string in the 
-- check result; UnknownUOM indicates a failure to parse.
data UOM = Second | Millisecond | Microsecond | Percent | Byte | Kilobyte | Megabyte | Gigabyte | Terabyte | Counter | NullUnit | UnknownUOM
    deriving (Eq)

instance Show UOM where
    show Second      = "s"
    show Percent     = "%"
    show Byte        = "B"
    show Counter     = "c"
    show NullUnit    = ""
    show UnknownUOM  = "?"
    show uom         = (show $ uomToPrefix uom) ++ (show $ uomToBase uom)

uomFromString :: String -> UOM
uomFromString "s"  = Second 
uomFromString "ms" = Millisecond
uomFromString "us" = Microsecond
uomFromString "%"  = Percent
uomFromString "B"  = Byte
uomFromString "KB" = Kilobyte
uomFromString "MB" = Megabyte
uomFromString "GB" = Gigabyte
uomFromString "TB" = Terabyte
uomFromString "c"  = Counter
uomFromString ""   = NullUnit
uomFromString _    = UnknownUOM

data Prefix = Base | Milli | Micro | Kilo | Mega | Giga | Tera
    deriving (Eq)

instance Show Prefix where
    show Base  = ""
    show Milli = "m"
    show Micro = "u"
    show Kilo  = "K"
    show Mega  = "M"
    show Giga  = "G"
    show Tera  = "T"

uomToPrefix :: UOM -> Prefix
uomToPrefix Second      = Base
uomToPrefix Millisecond = Milli
uomToPrefix Microsecond = Micro
uomToPrefix Percent     = Base
uomToPrefix Byte        = Base
uomToPrefix Kilobyte    = Kilo
uomToPrefix Megabyte    = Mega
uomToPrefix Gigabyte    = Giga
uomToPrefix Terabyte    = Tera
uomToPrefix Counter     = Base
uomToPrefix NullUnit    = Base
uomToPrefix UnknownUOM  = Base

uomToBase :: UOM -> UOM
uomToBase Second      = Second
uomToBase Millisecond = Second
uomToBase Microsecond = Second
uomToBase Percent     = Percent
uomToBase Byte        = Byte
uomToBase Kilobyte    = Byte
uomToBase Megabyte    = Byte
uomToBase Gigabyte    = Byte
uomToBase Terabyte    = Byte
uomToBase Counter     = Counter
uomToBase NullUnit    = NullUnit
uomToBase UnknownUOM  = UnknownUOM

prefixToScale :: Prefix -> Double
prefixToScale Base  = 1
prefixToScale Milli = 0.001
prefixToScale Micro = 0.000001
prefixToScale Kilo  = 1000
prefixToScale Mega  = 1000000
prefixToScale Giga  = 1000000000
prefixToScale Tera  = 1000000000000

uomToScale :: UOM -> Double
uomToScale = prefixToScale . uomToPrefix

convertUnitToBase :: MetricValue -> UOM -> (MetricValue, UOM)
convertUnitToBase UnknownValue uom = (UnknownValue, uom)
convertUnitToBase (DoubleValue v) uom = (DoubleValue $ uomToScale uom * v, uomToBase uom)

convertMetricToBase :: Metric -> Metric
convertMetricToBase m@Metric{..} = m{metricValue = v, metricUOM = uom}
    where
        (v, uom) = convertUnitToBase metricValue metricUOM

isMetricBase :: Metric -> Bool
isMetricBase Metric{..} = metricUOM == uomToBase metricUOM

-- |The part of the check result that's specific to service checks, 
-- and doesn't appear in host checks.
data ServicePerfdata = ServicePerfdata {
    serviceDescription :: S.ByteString,
    serviceState       :: ReturnState
} deriving (Show)

-- |The check type, either Service with associated ServiceData or Host.
data HostOrService = Service ServicePerfdata | Host deriving (Show)

data ReturnState = OKState | WarningState | CriticalState | UnknownState deriving (Show,Enum)

parseReturnCode :: Integral a => a -> Maybe ReturnState
parseReturnCode 0 = Just OKState
parseReturnCode 1 = Just WarningState
parseReturnCode 2 = Just CriticalState
parseReturnCode 3 = Just UnknownState
parseReturnCode _ = Nothing

parseReturnState :: S.ByteString -> Maybe ReturnState
parseReturnState "OK" = Just OKState
parseReturnState "WARNING" = Just WarningState
parseReturnState "CRITICAL" = Just CriticalState
parseReturnState "UNKNOWN" = Just UnknownState
parseReturnState _ = Nothing

-- |Encapsulates all the data in a check result that's relevant to 
-- metrics (we throw away things like the state type of HARD/SOFT). 
data Perfdata = Perfdata {
    perfdataType :: HostOrService,
    perfdataTimestamp :: Int64,
    perfdataHostname :: String,
    perfdataHostState :: Maybe S.ByteString,
    perfdataMetrics   :: MetricList
} deriving (Show)

perfdataServiceDescription :: Perfdata -> S.ByteString
perfdataServiceDescription datum = case perfdataType datum of
    Host -> "host"
    Service serviceData -> serviceDescription serviceData

uom :: Parser UOM
uom = liftM uomFromString . option "" $ many (satisfy uomChar)
  where
    uomChar = inClass "A-Za-z%"

metricName :: Parser String
metricName = option quote (char quote) *>
             many (satisfy nameChar) <*
             option quote (char quote)
  where
    quote = '\''
    nameChar '\'' = False
    nameChar '='  = False
    nameChar _    = True

value :: Parser MetricValue
value = option UnknownValue $ liftM DoubleValue double

threshold :: Parser Threshold
threshold = char8 ';' *> thresholdValue
            <|> thresholdValue
  where
    thresholdValue = option NoThreshold (liftM DoubleThreshold double)

metric :: Parser (String, Metric)
metric = do
    name <- metricName
    void $ char8 '='
    m    <- Metric `fmap` value <*>
                          uom <*>
                          threshold <*>
                          threshold <*>
                          threshold <*>
                          threshold 
    return (name, m)

metricLine :: Parser MetricList
metricLine = many (metric <* (skipMany (char8 ';') <* skipSpace))

-- |Parse the component of the check output which contains the 
-- performance metrics (HOSTPERFDATA or SERVICEPERFDATA). 
parseMetricString :: S.ByteString -> Either ParserError MetricList
parseMetricString = completeParse . parse metricLine
  where
    completeParse r = case r of
        Done _ m -> Right m
        Fail _ ctxs err -> Left $ fmtParseError ctxs err
        Partial parseRest -> completeParse (parseRest "")
