
module State (
  ServerState(..),
  Handler,
  RoutePattern,
  Route,
  HandlerTable,
  get,
  put,
  post,
  update,
  delete,
  all
) where

import qualified Control.Monad.State as ST
import qualified Control.Monad.Reader as R
import Prelude hiding (all)
import Request
import Response

type RoutePattern = String
type Route        = String
type Handler      = R.Reader Request Response
type HandlerTable = [(RoutePattern, RequestMethod, Handler)]

data ServerState =
  ServerState {
    handlers :: HandlerTable
  }

get :: RoutePattern -> Handler -> ST.State ServerState ()
get = addHandler GET

put :: RoutePattern -> Handler -> ST.State ServerState ()
put = addHandler PUT

post :: RoutePattern -> Handler -> ST.State ServerState ()
post = addHandler POST

update :: RoutePattern -> Handler -> ST.State ServerState ()
update = addHandler UPDATE

delete :: RoutePattern -> Handler -> ST.State ServerState ()
delete = addHandler DELETE

all :: RoutePattern -> Handler -> ST.State ServerState ()
all = addHandler ALL

addHandler :: RequestMethod -> RoutePattern -> Handler -> ST.State ServerState ()
addHandler method route handler = do
  (ServerState handlers) <- ST.get
  ST.put $ ServerState $ handlers ++ [(route, method, handler)]
