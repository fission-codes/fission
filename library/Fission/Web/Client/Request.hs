module Fission.Web.Client.Request
  ( request
  , withAuth
  ) where

import RIO

import qualified Network.HTTP.Client as HTTP
import           Servant
import           Servant.Client

withAuth :: HasClient ClientM api
         => Client ClientM api ~ (BasicAuthData -> clients)
         => BasicAuthData
         -> Proxy api
         -> clients
withAuth basicAuth proxy = client proxy basicAuth

request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
request manager url query = runClientM query $ mkClientEnv manager url
