
module Apollo (
  ServerState,
  apollo,
  status200,
  get,
  post,
  all,
) where

import qualified Control.Monad.State as ST
import qualified Control.Monad.Reader as R

import Prelude hiding (all)
import Text.Regex.Posix ((=~))
import Network (withSocketsDo, listenOn, PortID(..), PortNumber)
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll, recv) 
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Data.ByteString.Char8 (pack, unpack)

import Request
import Response
import State

initialState = ServerState []

-- apollo is the main entry point into the server
apollo :: Int -> ST.State ServerState () -> IO ()
apollo port actions = do
  sock <- listenOn $ PortNumber $ fromIntegral port
  putStrLn $ "Running Apollo on " ++ show port ++ "..."
  let state = ST.execState actions initialState
  forever $ do
    (conn, _) <- accept sock
    async $ handleConn conn state
  where makePort x = PortNumber x

-- handleConn is called in a new thread for each connection
-- handles turning the raw request plaintext into a proper
-- Request type
handleConn :: Socket -> ServerState -> IO ()
handleConn sock state = do
  putStrLn "New connection!"
  rawReq <- recv sock 4096
  putStrLn $ unpack rawReq
  let req = readRequest $ unpack rawReq
  case req of
    Just r  -> handleRequest sock r state
    _       -> putStrLn "Invalid request: could not parse request."

-- handleRequest finds the appropriate handler for the Request
handleRequest :: Socket -> Request -> ServerState -> IO ()
handleRequest sock req state = do
  let p = route req 
  let m = method req
  putStrLn $ "Request for route " ++ p
  putStrLn $ show $ map (\(x,y,z) -> x) $ handlers state
  let handler = lookupHandler p m (handlers state)
  case handler of
    Just h  -> sendAll sock . pack . serializeResponse $ R.runReader h req
    Nothing -> putStrLn $ "No handler for route " ++ p

-- finds a handler in the handler table
lookupHandler :: Route -> RequestMethod -> HandlerTable -> Maybe Handler
lookupHandler _ _ [] = Nothing
lookupHandler route method ((p, m, h):xs)
  | route =~ p && (method == m || m == ALL) = Just h
  | otherwise                               = lookupHandler route method xs
