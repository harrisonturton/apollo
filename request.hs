
module Request(
  Request(..),
	RequestMethod,
	readRequest
) where
import Data.List (isInfixOf, dropWhileEnd)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

data RequestMethod = GET | PUT | UPDATE | DELETE
  deriving Show

data Request =
	Request {
    uri     :: String,
		method  :: RequestMethod,
		headers :: [(String, String)],
		body    :: String
  } deriving Show


------------ Serialization ---------- 

serializeRequest :: Request -> String
serializeRequest req = serializeMethod req
	                   ++ " "
	                   ++ uri req
										 ++ " HTTP/1.1\n"
										 ++ serializeHeaders req
										 ++ "\n\n"
										 ++ body req

serializeMethod :: Request -> String
serializeMethod req =
	case method req of
		GET    -> "GET"
		PUT    -> "PUT"
		UPDATE -> "UPDATE"
		DELETE -> "DELETE"

serializeHeaders :: Request -> String
serializeHeaders =
	foldl (\acc (key, val) -> acc ++ "\n" ++ key ++ ": " ++ val) "" . headers

---------- Deserialization ---------- 

readRequest :: String -> Maybe Request
readRequest req =
	Request <$> readUri     req
					<*> readMethod  req
					<*> readHeaders req
					<*> readBody    req

readUri :: String -> Maybe String
readUri = Just . (!! 1) . words . head . lines

readMethod :: String -> Maybe RequestMethod
readMethod req =
  case method of
    "GET"    -> Just GET
    "PUT"    -> Just PUT
    "UPDATE" -> Just UPDATE
    "DELETE" -> Just DELETE
    _        -> Nothing
  where method  = (head . words . head . lines) req

readHeaders :: String -> Maybe [(String, String)]
readHeaders req = sequence . filter (/= Nothing) . map readOneHeader $ headers
  where headers = takeWhile (/= "") . tail . lines $ req

readBody :: String -> Maybe String
readBody req =
  case length body of
    0 -> Just ""
    _ -> Just $ unlines body
  where body = (dropWhile (/= "") . lines) req

readOneHeader :: String -> Maybe (String, String)
readOneHeader header
	| ": " `isInfixOf` header = Just (key, value)
  | ':'  `elem` header      = Just (key', value')
	| otherwise               = Nothing
  where [key, value]   = (splitOn ": " . strip) header
        [key', value'] = (splitOn ":"  . strip) header

-- Strip of leading & trailing whitespace
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- For testing
req = "GET /docs/index.html HTTP/1.1\nHost: www.nowhere123.com\nAccept: image/gif, image/jpeg, */*\nAccept-Language: en-us\nAccept-Encoding: gzip, deflate\nUser-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)\n\nBODY"
