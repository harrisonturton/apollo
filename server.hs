{-# LANGUAGE OverloadedStrings #-}

--import Network (withSocketsDo, listenOn, PortID(..))
--import Network.Socket (Socket, accept)
--import Network.Socket.ByteString (sendAll, send, recv)
--import Control.Concurrent (forkIO)
--import Control.Monad (forever)

import Network hiding (accept)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString hiding (putStrLn)
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
import Control.Monad (forever)

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
    (conn, _) <- accept sock
    forkIO $ handleAccept conn

handleAccept :: Socket -> IO ()
handleAccept sock = do
  putStrLn $ "Connected!"
  rawReq <- recv sock 1024
  putStrLn $ show rawReq
  send sock "Hello!\n"
  putStrLn $ (show . parseRequest . show) rawReq

parseRequest :: String -> Request
parseRequest reqLines =
	Request {
    reqType = (head . words . head . lines) reqLines,
		path    = ((!! 1) . words . head . lines) reqLines,
		options = (tail . lines) reqLines
  }
