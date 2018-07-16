
module State (
  ServerState,
	RoutePattern,
	Handler,
	Route
) where

import Control.Monad.State
import Request

data Response = Response String
  deriving Show

type RoutePattern = String
type Handler      = Request -> Response
type Route        = (RoutePattern, Handler)

data ServerState = ServerState { routes :: [Route] } deriving Show

-- So we can show Handler types
instance Show (a -> b) where
  show _ = "Handler"

-- Adds a new route. Updates the existing
-- route if a handler already exists for the
-- RoutePattern.
addRoute :: Route -> State ServerState ()
addRoute route = do
  (ServerState routes) <- get
  put $ ServerState $ upsertAssoc route routes

-- Add an new item to an associated list,
-- or replace it if it already exists
upsertAssoc :: Eq a => (a, b) -> [(a, b)] -> [(a, b)]
upsertAssoc elem assoc =
	case lookup key assoc of
    Just _  -> elem : (removeAssoc key assoc)
    Nothing -> elem : assoc
  where key = fst elem

-- Remove an item from an associated list
removeAssoc :: Eq a => a -> [(a, b)] -> [(a, b)]
removeAssoc key = filter ((/= key) . fst)

handler :: Request -> Response
handler _ = Response "Done!"

initState = ServerState [("/", handler)]

main = do
  let r = runState (addRoute ("/woo/", handler)) initState
  putStrLn $ show r
