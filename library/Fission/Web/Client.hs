module Fission.Web.Client
  -- ( request
  -- , sigClient
  -- , sigClient'
  -- , basicClient
  -- , registerClient
  -- -- , is404
  ( MonadWebRequest     (..)
  , MonadAuthedEndpoint (..)
  , HasWebAuth          (..)
  , withAuth
  -- , module Fission.Web.Client.Types
  -- , module Fission.Web.Client.Class
  ) where

import           Fission.Prelude

-- import           Servant
import           Servant.Client
-- import           Servant.Client.Core

-- import qualified Network.HTTP.Client as HTTP
-- import           Network.HTTP.Types.Status

-- import           Fission.Web.Client.Types
-- import           Fission.Web.Client.Class
-- import           Fission.Web.Client.BasicAuth as Auth

-- import           Fission.Web.Client.JWT as JWT
-- import           Fission.Web.Auth.Types as Auth

import Fission.Web.Auth.Token.JWT
import Fission.User.DID

-- request :: MonadClient m => m a -> m (Either ClientError a)
-- request manager url query = runClientM query $ mkClientEnv manager url
-- request :: HTTP.Manager -> BaseUrl -> ClientM a -> IO (Either ClientError a)
-- request manager url query = runClientM query $ mkClientEnv manager url

class Monad m => MonadWebRequest request m | m -> request where
  sendRequest :: request a -> m (Either ClientError a)

class HasWebAuth m => MonadAuthedEndpoint api m where
-- sendRequest $ toEndpoint (Proxy @NewPinAPI) do
--   return constuctArgs
--   return constuctArgs
--   return constuctArgs
  -- toEndpoint :: Proxy api -> auth -> arg -> m a
  toEndpoint :: Proxy api -> arg -> auth -> m a
  -- toUnauthedEndpoint :: Proxy api ->         arg -> m a -- is Auth ehere just ()?

class Monad m => HasWebAuth m where
  ucanJWT :: m JWT
  rawDID  :: m DID

-- toEndpoint' :: MonadAuthedEndpoint api m => Proxy api -> m result
-- toEndpoint' endpointPxy = toEndpoint endpointPxy identity
withAuth :: HasWebAuth m => m auth -> (auth -> m a) -> m a
withAuth auth req = req =<< auth

{-
sendRequest . withAuth ucan $ toEndpoint do
  args
-}

-- withDID' :: MonadAuthedEndpoint api m => Proxy api -> m result
-- withDID' endpointPxy = withDID endpointPxy identity

-- withDID :: MonadAuthedEndpoint api m => Proxy api -> arg -> m result
-- withDID endpointPxy arg = do
--   auth <- rawDID
--   toEndpoint endpointPxy auth arg

-- withUCAN' endpointPxy =

-- withUCAN :: MonadAuthedEndpoint api m => Proxy api -> arg -> m result
-- withUCAN endpointPxy arg = do
--   auth <- ucanJWT
--   toEndpoint endpointPxy auth arg

-- basicClient ::
--   ( HasClient ClientM api
--   , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> a -> ClientM b)
--   )
--   => Proxy api
--   -> BasicAuthData
--   -> a
--   -> ClientM b
-- basicClient p auth x = (client p) (Auth.getBasicAuth auth) x

-- sigClient ::
--   ( HasClient ClientM api
--   , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> a -> ClientM b)
--   )
--   => Proxy api
--   -> a
--   -> ClientM b
-- sigClient p x = do
--   jwt <- JWT.getSigAuth
--   (client p) jwt x
--

-- sigClient' ::
--   ( HasClient ClientM api
--   , Client ClientM api ~ (AuthenticatedRequest Auth.HigherOrder -> ClientM b)
--   )
--   => Proxy api
--   -> ClientM b
-- sigClient' p = JWT.getSigAuth >>= client p

-- registerClient ::
--   ( HasClient ClientM api
--   , Client ClientM api ~ (AuthenticatedRequest Auth.RegisterDID -> a -> ClientM b)
--   )
--   => Proxy api
--   -> a
--   -> ClientM b
-- registerClient p x = JWT.getRegisterAuth >>= flip (client p) x

-- is404 :: ClientError -> Bool
-- is404 (FailureResponse _ resp) = responseStatusCode resp == status404
-- is404 _ = False
