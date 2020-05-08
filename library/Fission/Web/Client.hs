module Fission.Web.Client
  ( sendRequestM
  , withPayload
  , authClient
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Servant.Client
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID
 
import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class
import           Fission.Web.Client.JWT

sendRequestM :: MonadWebClient m => m (ClientM a) -> m (Either ClientError a)
sendRequestM clientAction = sendRequest =<< clientAction

authClient ::
  ( MonadIO      m
  , MonadTime    m
  , ServerDID    m
  , MonadWebAuth m (AuthClientData a)
  , MonadWebAuth m Ed25519.SecretKey
  , HasClient ClientM api
  , Client    ClientM api ~ (AuthenticatedRequest a -> f b)
  )
  => Proxy api
  -> m (f b)
authClient pxy = do
  auth    <- getAuth
  authReq <- mkAuthReq
  return . (client pxy) $ mkAuthenticatedRequest auth \_ath -> authReq

infixl 1 `withPayload`
withPayload :: Functor f => f (a -> b) -> a -> f b
clientFun `withPayload` arg = (\f -> f arg) <$> clientFun

