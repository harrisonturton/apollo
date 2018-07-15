{-# LANGUAGE OverloadedStrings #-}

import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, accept)
import Network.Socket.ByteString (sendAll, send, recv)
import Control.Concurrent.Async (async)
import Control.Monad (forever)

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3000
  putStrLn "Listening on port 3000..."
  forever $ do
    (conn, _) <- accept sock
    async $ handleAccept conn

handleAccept :: Socket -> IO ()
handleAccept sock = do
  putStrLn $ "Connected!"
  rawReq <- recv sock 1024
  putStrLn $ show rawReq
  sendAll sock "Hello!\n"
