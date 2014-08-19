-- This file is part of nagios-perfdata.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE OverloadedStrings #-}

module Data.Nagios.Perfdata.Error(
    fmtParseError,
    ParserError
) where

import Prelude hiding (takeWhile)
import Data.List hiding (takeWhile)

type ParserError = String

fmtParseError :: [String] -> String -> String
fmtParseError ctxs err = err ++ intercalate "," ctxs
