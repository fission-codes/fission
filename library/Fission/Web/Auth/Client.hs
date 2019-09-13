module Fission.Web.Auth.Client (run, verify) where

import RIO

import Data.Has

import qualified Network.HTTP.Client as HTTP

import Servant
import Servant.Client

import qualified Fission.Config              as Config
import           Fission.Internal.Constraint

import qualified Fission.Web.Routes as Web
import qualified Fission.Web.Types  as Web

verify :: BasicAuthData -> ClientM Bool
verify = client (Proxy :: Proxy Web.AuthRoute)

-- run :: MonadRIO         cfg m
--     => Has Web.Host     cfg
--     => Has HTTP.Manager cfg
--     => ClientM a
--     -> m (Either ServantError a)
-- run query = do
--   manager       <- Config.get
--   -- Web.Host host <- Config.get
--   let url = BaseUrl Http "loalhost" 1337 ""
--   liftIO . runClientM query $ mkClientEnv manager url

run :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ServantError a)
run manager url query = runClientM query $ mkClientEnv manager url
