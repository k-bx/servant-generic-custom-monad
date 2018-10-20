{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
  ( main
  , api
  , getLink
  , routesLinks
  , cliGet
  ) where

import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic
import Servant.Server.Generic
import System.Environment (getArgs)

-- | Transform a record of routes with custom monad into a WAI 'Application',
--   by providing a transformation to bring each handler back in the 'Handler'
--   monad.
genericServeT
  :: forall (routes :: * -> *) (m :: * -> *).
     ( GenericServant routes (AsServerT m)
     , GenericServant routes AsApi
     , HasServer (ToServantApi routes) '[]
     , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
     )
  => (forall a. m a -> Handler a) -- ^ 'hoistServer' argument to come back to 'Handler'
  -> routes (AsServerT m)         -- ^ your record full of request handlers
  -> Application
genericServeT f server = serve p $ hoistServer p f (genericServerT server)

  where p = genericApi (Proxy :: Proxy routes)

-- | Transform a record of routes with custom monad into a WAI 'Application',
--   while using the given 'Context' to serve the application (contexts are typically
--   used by auth-related combinators in servant, e.g to hold auth checks) and the given
--   transformation to map all the handlers back to the 'Handler' monad.
genericServeT'
  :: forall (routes :: * -> *) (m :: * -> *) (ctx :: [*]).
     ( GenericServant routes (AsServerT m)
     , GenericServant routes AsApi
     , HasServer (ToServantApi routes) ctx
     , ServerT (ToServantApi routes) m ~ ToServant routes (AsServerT m)
     )
  => (forall a. m a -> Handler a) -- ^ 'hoistServer' argument to come back to 'Handler'
  -> routes (AsServerT m)         -- ^ your record full of request handlers
  -> Context ctx                  -- ^ the 'Context' to serve the application with
  -> Application
genericServeT' f server ctx = serveWithContext p ctx $
  hoistServerWithContext p pctx f (genericServerT server)

  where p = genericApi (Proxy :: Proxy routes)
        pctx = Proxy :: Proxy ctx

data AppCustomState =
  AppCustomState

type AppM = ReaderT AppCustomState Handler

data Routes route = Routes
  { _get :: route :- Capture "id" Int :> Get '[ JSON] String
  , _put :: route :- ReqBody '[ JSON] Int :> Put '[ JSON] Bool
  } deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

getLink :: Int -> Link
getLink = fieldLink _get

routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks

cliRoutes :: Routes (AsClientT IO)
cliRoutes =
  genericClientHoist (\x -> runClientM x env >>= either throwIO return)
  where
    env = error "undefined environment"

cliGet :: Int -> IO String
cliGet = _get cliRoutes

getRoute :: Int -> AppM String
getRoute = return . show

putRoute :: Int -> AppM Bool
putRoute = return . odd

record :: Routes (AsServerT AppM)
record = Routes {_get = getRoute, _put = putRoute}

nt :: AppCustomState -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppCustomState -> Application
app state = genericServeT (nt state) record

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run":_) -> do
      putStrLn "Starting cookbook-generic at http://localhost:8000"
      run 8000 (app AppCustomState)
    _ ->
      putStrLn "To run, pass 'run' argument: cabal new-run cookbook-generic run"
