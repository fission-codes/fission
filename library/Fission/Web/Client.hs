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
  , toEndpoint'
  -- , module Fission.Web.Client.Types
  -- , module Fission.Web.Client.Class
  ) where

import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client.Auth

class Monad m => MonadWebRequest request m | m -> request where
  sendRequest :: request a -> m (Either ClientError a)

class HasWebAuth m => MonadAuthedEndpoint api m where
-- sendRequest $ toEndpoint (Proxy @NewPinAPI) do
--   return constuctArgs
--   return constuctArgs
--   return constuctArgs
  -- toEndpoint :: Proxy api -> auth -> arg -> m a
  toEndpoint :: Proxy api -> arg -> auth -> m a

toEndpoint' :: MonadAuthedEndpoint api m => Proxy api -> auth -> m a
toEndpoint' endpointPxy auth = toEndpoint endpointPxy identity auth
