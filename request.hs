{-# LANGUAGE OverloadedStrings #-}

module Request (
	Request,
	RequestType,
  parseRawRequest,
	parseRawRequestType,
	parseRawRequestPath,
	parseRawRequestOps
) where

import Debug.Trace
import Data.List (isInfixOf, dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Char (isSpace)

data RequestType = GET | PUT
  deriving Show

data Request =
	Request {
    reqType :: RequestType,
		path    :: String,
		options :: [(String, String)]
  } deriving Show

-- Turn a raw HTTP request into a request
-- object.
parseRawRequest :: String -> Maybe Request
parseRawRequest rawReq =
  Request <$> parseRawRequestType rawReq
	        <*> parseRawRequestPath rawReq
					<*> parseRawRequestOps  rawReq

-- Turn an (entire) raw HTTP request into just
-- the request type.
parseRawRequestType :: String -> Maybe RequestType
parseRawRequestType rawReq = 
	case typ of
    "GET" -> Just GET
    "PUT" -> Just PUT
    _     -> Nothing
  where typ = (head . words . head . lines) rawReq

-- Turn an (entire) raw HTTP request into just
-- the path.
parseRawRequestPath :: String -> Maybe String
parseRawRequestPath = Just . (!! 1) . words . head . lines

-- Turn an (entire) raw HTTP request into just
-- a lookup table of their options.
parseRawRequestOps :: String -> Maybe [(String, String)]
parseRawRequestOps = sequence . filter (/= Nothing) . map parseSingleOption . tail . lines . strip

parseSingleOption :: String -> Maybe (String, String)
parseSingleOption line
	| ": " `isInfixOf` line = Just (key, value)
  | ':' `elem` line       = Just (key', value')
	| otherwise             = trace (show line) Nothing
 where [key, value]   = (splitOn ": " . strip) line
       [key', value'] = (splitOn ":"  . strip) line

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace
