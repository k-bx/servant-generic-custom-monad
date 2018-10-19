{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  , api
  , getLink
  , routesLinks
  , cliGet
  ) where

import Control.Exception (throwIO)
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic
import Servant.Server.Generic
import System.Environment (getArgs)

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

record :: Routes AsServer
record = Routes {_get = return . show, _put = return . odd}

app :: Application
app = genericServe record

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run":_) -> do
      putStrLn "Starting cookbook-generic at http://localhost:8000"
      run 8000 app
    _ ->
      putStrLn "To run, pass 'run' argument: cabal new-run cookbook-generic run"
