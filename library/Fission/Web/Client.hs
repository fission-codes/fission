module Fission.Web.Client (request) where

import RIO

import qualified Network.HTTP.Client as HTTP

import Servant.Client

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ServantError a)
request manager url query = runClientM query $ mkClientEnv manager url
