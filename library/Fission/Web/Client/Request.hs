module Fission.Web.Client.Request (request) where

import RIO

import qualified Network.HTTP.Client as HTTP

import Servant.Client

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
request manager url query = runClientM query $ mkClientEnv manager url
