-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}

module Data.Nagios.Perfdata.CheckResult(
    perfdataFromCheckResult,
) where

import Data.Nagios.Perfdata.Metric
import Data.Nagios.Perfdata.Error

import Prelude hiding (takeWhile)
import Data.Int
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

type CheckResultField = (String,String)

checkResultSep :: Parser String
checkResultSep = many1 (char '\n')

checkResultFieldName :: Parser String
checkResultFieldName = manyTill anyChar (char '=')

checkResultFieldValue :: Parser String
checkResultFieldValue = manyTill anyChar checkResultSep

checkResultField :: Parser CheckResultField
checkResultField = (,) `fmap` checkResultFieldName <*> checkResultFieldValue

checkResult :: Parser [CheckResultField]
checkResult = many (char '"') *> many1 checkResultField <* many (char '"')

type CheckResultMap = M.Map String String

mapResultItems :: [CheckResultField] -> CheckResultMap
mapResultItems = foldl (flip (uncurry M.insert)) M.empty

extractResultItems :: Result [CheckResultField] -> Either ParserError CheckResultMap
extractResultItems (Done _ is) = Right $ mapResultItems is
extractResultItems (Fail _ ctxs err) = Left $ fmtParseError ctxs err
extractResultItems (Partial f) = extractResultItems (f "")

checkTimestamp :: CheckResultMap -> Either ParserError Int64
checkTimestamp m = 
    case M.lookup "finish_time" m of
        Nothing -> Left "finish_time not found"
        Just t  -> do
            x <- parseDouble (C.pack t)
            return $ floor  $ x * nanosecondFactor
  where
    nanosecondFactor = 1000000000

parseDouble :: C.ByteString -> Either ParserError Double
parseDouble s = complete (parse double s)
  where
    complete (Done _ i) = Right i
    complete (Fail _ ctxs err) = Left $ fmtParseError ctxs err
    complete (Partial f) = complete (f "")

parsePluginOutput :: S.ByteString -> Either ParserError String
parsePluginOutput s = complete (parse metricSection s)
  where
    complete (Done _ i) = Right i
    complete (Fail _ ctxs err) = Left $ fmtParseError ctxs err
    complete (Partial f) = complete (f "")
    metricSection = manyTill anyChar (char '|') *> many1 anyChar

checkMetrics :: CheckResultMap -> Either ParserError MetricList
checkMetrics m = 
    case M.lookup "output" m of
        Nothing -> Left "check output not found"
        Just s -> do
            metricPart <- parsePluginOutput (C.pack s)
            parseMetricString (C.pack metricPart)

checkHostname :: CheckResultMap -> Either ParserError String
checkHostname m = 
    case M.lookup "host_name" m of
        Nothing -> Left "host_name not found"
        Just h -> Right h

checkServiceState :: CheckResultMap -> Either ParserError ReturnState
checkServiceState m = 
    case M.lookup "return_code" m of
        Nothing -> Left "return_code not found"
        Just d  -> case C.readInteger (C.pack d) of 
            Nothing -> Left "could not parse return code as an integer"
            Just (r,_) -> case parseReturnCode r of
                Nothing -> Left "invalid return code"
                Just rc  -> Right rc

checkType :: CheckResultMap -> Either ParserError HostOrService
checkType m = 
    case M.lookup "service_description" m of
        Nothing -> Right Host
        Just sd -> do
            sd' <- Right (C.pack sd)
            state <-  checkServiceState m
            return $ Service $ ServicePerfdata sd' state

extractCheckItems :: S.ByteString -> Either ParserError CheckResultMap
extractCheckItems = extractResultItems . parse checkResult

-- |Takes the output of a Nagios check formatted according to [0] and 
-- attempts to parse it into a Perfdata object. This should be used, for
-- example, for consuming perfdata from mod_gearman check_result queues. 
--
-- [0]: https://nagios-plugins.org/doc/guidelines.html
perfdataFromCheckResult :: S.ByteString -> Either ParserError Perfdata
perfdataFromCheckResult s = do
    m <- extractCheckItems s
    typ <- checkType m
    name <- checkHostname m
    t <- checkTimestamp m
    state <- Right Nothing
    ms <- checkMetrics m
    return $ Perfdata typ t name state ms

