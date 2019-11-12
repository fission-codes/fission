module Fission.Web.Tls
  ( run
  , url
  ) where

import Network.HTTP.Client
import Servant.Client

import Fission.Prelude

run :: Manager -> ClientM a -> String -> IO (Either ClientError a)
run mgr clnt = runClientM clnt . mkClientEnv mgr . url

url :: String -> BaseUrl
url loc = BaseUrl Https loc 443 ""
