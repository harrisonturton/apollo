
import qualified Control.Monad.State as ST
import qualified Control.Monad.Reader as R
import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll, recv) 
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Data.ByteString.Char8 (pack, unpack)
import Request
import Response

type Handler = R.Reader Request Response
type RoutePattern = String

data ServerState =
  ServerState {
		handlers :: [(String, Handler)]
  }

initState = ServerState []

-- apollo is the main entry point into the server
apollo :: PortID -> ST.State ServerState () -> IO ()
apollo port actions = do
  sock <- listenOn $ port
  putStrLn $ "Running Apollo on " ++ show port ++ "..."
  let state = ST.execState actions initState
  forever $ do
    (conn, _) <- accept sock
    async $ handleConn conn state

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
  let p = uri req 
  putStrLn $ "Request for route " ++ p
  let handler = lookup p (handlers state)
  case handler of
    Just h  -> sendAll sock . pack . serializeResponse $ R.runReader h req
    Nothing -> putStrLn $ "No handler for route " ++ p

-- adds a handler for GET requests at a specific route
get :: RoutePattern -> Handler -> ST.State ServerState ()
get route handler = do
  (ServerState handlers) <- ST.get
  ST.put $ ServerState $ (route, handler):handlers

main = apollo (PortNumber 3000) $ do
  get "/" $ do
    req <- R.ask
    return $ status200 "Hello!"
