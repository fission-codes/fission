module Fission.Web.Tls
  ( run
  , url
  ) where

import           Network.HTTP.Client
import           Servant.Client

import           Fission.Prelude

run :: MonadIO m => Manager -> ClientM a -> String -> m (Either ClientError a)
run mgr clnt = liftIO . runClientM clnt . mkClientEnv mgr . url

url :: String -> BaseUrl
url loc = BaseUrl Https loc 443 ""
