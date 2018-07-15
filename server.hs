
import Control.Monad
import System.IO
import Network
import Control.Concurrent

data Request =
	Request {
    reqType :: String,
		path    :: String,
		options :: [String]
  } deriving Show

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3000
  putStrLn "Listening on port 3000..."
  forever $ do
    (handle, hostname, _) <- accept sock
    handleAccept handle hostname

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do
  putStrLn $ "Handling request from " ++ hostname
  reqLines <- hGetContents handle
  putStrLn $ show reqLines
  putStrLn $ (show . parseRequest) reqLines

parseRequest :: String -> Request
parseRequest reqLines =
	Request {
    reqType = (head . words . head . lines) reqLines,
		path    = ((!! 1) . words . head . lines) reqLines,
		options = (tail . lines) reqLines
  }
