
import qualified Control.Monad.State as ST
import qualified Control.Monad.Reader as R
import Network (withSocketsDo, listenOn, PortID(..))
import Network.Socket (Socket, accept, close)
import Network.Socket.ByteString (sendAll, recv) 
import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack)
import Request
import Response

type Handler = R.Reader Request String
type RoutePattern = String

data ServerState =
  ServerState {
		handlers :: [(String, Handler)]
  }

initState = ServerState []

apollo :: PortID -> ST.State ServerState () -> IO ()
apollo port actions = do
  sock <- listenOn $ port
  putStrLn $ "Running Apollo on " ++ show port ++ "..."
  let state = ST.execState actions initState
  forever $ do
    (conn, _) <- accept sock
    async $ handleConn conn state

handleConn :: Socket -> ServerState -> IO ()
handleConn sock state = do
  putStrLn "New connection!"
  rawReq <- recv sock 4096
  putStrLn $ unpack rawReq
  let req = parseRawRequest $ unpack rawReq
  case req of
    Just r -> handleRequest r state
    Nothing -> putStrLn "Invalid request: could not parse request."

get :: RoutePattern -> Handler -> ST.State ServerState ()
get route handler = do
  (ServerState handlers) <- ST.get
  ST.put $ ServerState $ (route, handler):handlers

handleRequest :: Request -> ServerState -> IO ()
handleRequest req state = do
  let p = path req 
  putStrLn $ "Request for route " ++ p
  let handler = lookup p (handlers state)
  case handler of
		Just h  -> putStrLn $ show $ R.runReader h req
		Nothing -> putStrLn $ "No handler for route " ++ p

main = apollo (PortNumber 3000) $ do
  get "/" $ do
    req <- R.ask
    return "Done"
