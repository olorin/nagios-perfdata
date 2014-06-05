-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}

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
    perfdataServiceDescription
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
data MetricValue = DoubleValue Double | UnknownValue deriving (Show)

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

-- |List of metrics by metric name.
type MetricList = [(String, Metric)]

-- |Nagios unit of measurement. NullUnit is an empty string in the 
-- check result; UnknownUOM indicates a failure to parse.
data UOM = Second | Millisecond | Microsecond | Percent | Byte | Kilobyte | Megabyte | Terabyte | Counter | NullUnit | UnknownUOM
    deriving (Show)

uomFromString :: String -> UOM
uomFromString "s" = Second 
uomFromString "ms" = Millisecond
uomFromString "us" = Microsecond
uomFromString "%" = Percent
uomFromString "b" = Byte
uomFromString "kb" = Kilobyte
uomFromString "mb" = Megabyte
uomFromString "tb" = Terabyte
uomFromString "c" = Counter
uomFromString "" = NullUnit
uomFromString _ = UnknownUOM

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
perfdataServiceDescription datum = case (perfdataType datum) of
    Host -> "host"
    Service serviceData -> serviceDescription serviceData

uom :: Parser UOM
uom = option "" (many letter_ascii) >>= (return . uomFromString)

metricName :: Parser [Char]
metricName = (option quote (char quote)) *>
             (many (satisfy nameChar)) <*
             (option quote (char quote))
  where
    quote = '\''
    nameChar '\'' = False
    nameChar '='  = False
    nameChar _    = True

value :: Parser MetricValue
value = option UnknownValue (double >>= (return . DoubleValue))

threshold :: Parser Threshold
threshold = (char8 ';') *> option NoThreshold (double >>= (return . DoubleThreshold))

metric :: Parser ([Char], Metric)
metric = do
    name <- metricName
    void $ char8 '='
    m    <- Metric `fmap` value <*>
                          uom <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) <*>
                          (option NoThreshold threshold) 
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
