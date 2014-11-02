-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}

module Data.Nagios.Perfdata.Template(
    perfdataFromDefaultTemplate,
) where

import           Data.Nagios.Perfdata.Error
import           Data.Nagios.Perfdata.Metric

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as S
import           Data.ByteString.Char8            (readInteger)
import qualified Data.ByteString.Char8            as C
import           Data.Int
import qualified Data.Map                         as M
import           Data.Word
import           Prelude                          hiding (takeWhile)

data Item = Item {
    label   :: S.ByteString,
    content :: S.ByteString
} deriving (Show)

-- |Matches the '::' separating items in check result output.
separator :: Parser [Word8]
separator = count 2 (char8 ':') <?> "separator"

-- |Matches the key in check result output.
ident :: Parser S.ByteString
ident = takeWhile uppercase <?> "item identifier"
  where
    uppercase = inClass $ enumFromTo 'A' 'Z'

-- |Matches the value in check result output.
val :: Parser S.ByteString
val = takeTill isTabOrEol <?> "item value"
  where
    isTabOrEol c = c == '\t' || c == '\n'

-- |Matches a key::value pair in check result output.
item :: Parser Item
item = Item `fmap` ident <* separator <*> val <* skipWhile isTab <?> "perfdata item"
  where
    isTab = (==) '\t'

-- |Matches a line of key::value pairs (i.e., the result of one check).
line :: Parser [Item]
line = many item <?> "perfdata line"

-- |Map from key to value for items in a check result.
type ItemMap = M.Map S.ByteString S.ByteString

-- |Insert items from a list into a map for easy access by key.
mapItems :: [Item] -> ItemMap
mapItems = foldl (\m i -> M.insert (label i) (content i) m) M.empty

-- |Parse the output from a Nagios check.
parseLine :: S.ByteString -> Result [Item]
parseLine = parse line

-- |We have no more data to give the parser at this point, so we
-- either fail or succeed here and return a ParserError or an ItemMap
-- respectively.
extractItems :: Result [Item] -> Either ParserError ItemMap
extractItems (Done _ is) = Right $ mapItems is
extractItems (Fail _ ctxs err) = Left $ fmtParseError ctxs err
extractItems (Partial f) = extractItems (f "")

-- |Called if the check output is from a service check. Returns the
-- service-specific component of the perfdata.
parseServiceData :: ItemMap -> Either ParserError ServicePerfdata
parseServiceData m = case M.lookup "SERVICEDESC" m of
    Nothing -> Left ("SERVICEDESC not found in " ++ show m)
    Just desc -> case M.lookup "SERVICESTATE" m of
        Nothing -> Left "SERVICESTATE not found"
        Just sState -> case parseReturnState sState of
            Nothing -> Left ("invalid service state " ++ C.unpack sState)
            Just st -> Right $ ServicePerfdata desc st

-- |Whether this perfdata item is for a host check or a service check
-- (or Nothing on failure to determine).
parseDataType :: ItemMap -> Either ParserError HostOrService
parseDataType m = case M.lookup "DATATYPE" m of
    Nothing -> Left "DATATYPE not found"
    Just s -> case s of
        "HOSTPERFDATA" -> Right Host
        "SERVICEPERFDATA" -> case parseServiceData m of
            Left err -> Left err
            Right d -> Right $ Service d
        _                 -> Left "Invalid datatype"

parseHostname :: ItemMap -> Either ParserError S.ByteString
parseHostname m = case M.lookup "HOSTNAME" m of
    Nothing -> Left "HOSTNAME not found"
    Just h -> Right h

parseTimestamp :: ItemMap -> Either ParserError Int64
parseTimestamp m = case M.lookup "TIMET" m of
    Nothing -> Left "TIMET not found"
    Just t  -> case readInteger t of
        Nothing -> Left "Invalid timestamp"
        Just (n, _) -> Right $ fromInteger (n * nanosecondFactor)
  where
    nanosecondFactor = 1000000000

parseHostState :: ItemMap -> Either ParserError S.ByteString
parseHostState m = case M.lookup "HOSTSTATE" m of
    Nothing -> Left "HOSTSTATE not found"
    Just s -> Right s

parseHostMetrics :: ItemMap -> Either ParserError MetricList
parseHostMetrics m = case M.lookup "HOSTPERFDATA" m of
    Nothing -> Left "HOSTPERFDATA not found"
    Just p  -> parseMetricString p

parseServiceMetrics :: ItemMap -> Either ParserError MetricList
parseServiceMetrics m = case M.lookup "SERVICEPERFDATA" m of
    Nothing -> Left "SERVICEPERFDATA not found"
    Just p  -> parseMetricString p

-- |Given an item map extracted from a check result, parse and return
-- the performance metrics (or store an error and return Nothing).
parseMetrics :: HostOrService -> ItemMap -> Either ParserError MetricList
parseMetrics typ m = case typ of
     Host -> parseHostMetrics m
     Service _ -> parseServiceMetrics m

-- |Given an item map extracted from a check result, parse and return
-- a Perfdata object.
extractPerfdata :: ItemMap -> Either ParserError Perfdata
extractPerfdata m = do
    typ <- parseDataType m
    name <- parseHostname m
    t <- parseTimestamp m
    state <- parseHostState m
    ms <- parseMetrics typ m
    return $ Perfdata typ t (C.unpack name) (Just state) ms

-- |Extract perfdata from a Nagios perfdata item formatted according to
-- the default template[0]. This is the format that is used in the
-- perfdata spool files and consumed by pnp4nagios.
--
-- [0] Default templates defined in the Nagios source (xdata/xpddefault.h).
-- Service perfdata:
--   "[SERVICEPERFDATA]\t$TIMET$\t$HOSTNAME$\t$SERVICEDESC$\t$SERVICEEXECUTIONTIME$\t$SERVICELATENCY$\t$SERVICEOUTPUT$\t$SERVICEPERFDATA$"
-- Host perfdata:
--   "[HOSTPERFDATA]\t$TIMET$\t$HOSTNAME$\t$HOSTEXECUTIONTIME$\t$HOSTOUTPUT$\t$HOSTPERFDATA$"
perfdataFromDefaultTemplate :: S.ByteString -> Either ParserError Perfdata
perfdataFromDefaultTemplate s =
    getItems s >>= extractPerfdata
  where
    getItems = extractItems . parseLine
