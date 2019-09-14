module Fission.Web.Auth.Client (run, verify) where

import RIO

import qualified Network.HTTP.Client as HTTP

import Servant
import Servant.Client

import qualified Fission.Web.Routes as Web

verify :: BasicAuthData -> ClientM Bool
verify = client (Proxy :: Proxy Web.AuthRoute)

run :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ServantError a)
run manager url query = runClientM query $ mkClientEnv manager url
