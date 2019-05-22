{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Web.Tls
  ( run
  , url
  ) where

import Network.HTTP.Client
import RIO
import Servant.Client

run :: Manager -> ClientM a -> String -> IO (Either ServantError a)
run mgr clnt = runClientM clnt . mkClientEnv mgr . url

url :: String -> BaseUrl
url loc = BaseUrl Https loc 443 ""
