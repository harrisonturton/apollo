{-# LANGUAGE OverloadedStrings #-}

import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll, recv) 
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack)
import Request

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3000
  putStrLn "Listening on port 3000..."
  forever $ do
    (conn, _) <- accept sock
    async $ handleAccept conn

handleAccept :: Socket -> IO ()
handleAccept sock = do
	putStrLn $ "New connection!"
  rawReq <- recv sock 1024
  let req = parseRawRequest $ unpack rawReq
  case req of
		Just r  -> handleRequest sock r
		Nothing -> putStrLn "Invalid request: failed to parse Request."

handleRequest :: Socket -> Request -> IO ()
handleRequest sock req = do
	sendAll sock "In handleRequest!\n" -- Must have newline otherwise doesn't appear in client
