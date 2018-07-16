{-# LANGUAGE OverloadedStrings #-}

import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll, recv) 
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack)
import Request
import Response

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
  putStrLn $ show req
  case req of
    Just r  -> handleRequest sock r
    Nothing -> do
      putStrLn "Invalid request: failed to parse Request."
      sendAll sock rawReq

handleRequest :: Socket -> Request -> IO ()
handleRequest sock req = do
  --let response = route req router
  sendAll sock "In handleRequest!\n" 
  --close sock -- Must close sock otherwise data isn't sent
