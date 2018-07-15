
module Request (
	Request,
	RequestType,
  parseRawRequest,
	parseRawRequestType,
	parseRawRequestPath,
	parseRawRequestOps
) where

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
parseRawRequestOps rawReq = Just [("One", "Two")]
