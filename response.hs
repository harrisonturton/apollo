
module Response (
  Response(..),
  serializeResponse,
  status200,
  status404,
  status500
) where

data Response =
  Response {
    statusCode    :: Int,
    statusMessage :: String,
    headers       :: [(String, String)],
    body          :: String
  } deriving Show

status200 :: String -> Response
status200 = Response 200 "OK" []

status404 :: String -> Response
status404 = Response 404 "Not Found" []

status500 :: String -> Response
status500 = Response 500 "Internal Server Error" []

serializeResponse :: Response -> String
serializeResponse res = "HTTP/1.1 "
                      ++ (show . statusCode) res
                      ++ " "
                      ++ statusMessage res
                      ++ "\n"
                      ++ serializeHeaders res
                      ++ "\n"
                      ++ body res

serializeHeaders :: Response -> String
serializeHeaders =
  foldl (\acc (key, val) -> acc ++ "\n" ++ key ++ ": " ++ val) "" . headers
