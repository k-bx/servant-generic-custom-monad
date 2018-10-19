{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
app state =
  serve
    (Proxy :: Proxy (ToServantApi Routes))
    (hoistServer
       (Proxy :: Proxy (ToServantApi Routes))
       (nt state)
       (genericServerT record))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run":_) -> do
      putStrLn "Starting cookbook-generic at http://localhost:8000"
      run 8000 (app AppCustomState)
    _ ->
      putStrLn "To run, pass 'run' argument: cabal new-run cookbook-generic run"
