module Fission.Web.Client
  ( sendRequestM
  , withPayload
  , authClient
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import qualified Crypto.PubKey.Ed25519           as Ed25519

import           Servant.Client
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID

import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class
import           Fission.Web.Client.JWT

sendRequestM ::
  ( MonadWebClient m
  , MonadRaise     m
  ,  m `Raises` ClientError
  )
  => m (ClientM a)
  -> m a
sendRequestM clientAction = ensureM (sendRequest =<< clientAction)

authClient ::
  ( MonadIO      m
  , MonadTime    m
  , MonadLogger  m
  , ServerDID    m
  , MonadWebAuth m (AuthClientData a)
  , MonadWebAuth m Ed25519.SecretKey
  , HasClient ClientM api
  , Client    ClientM api ~ (AuthenticatedRequest a -> f b)
  )
  => Proxy api
  -> m (f b)
authClient api = do
  auth    <- getAuth
  authReq <- mkAuthReq

  logDebug @Text "Sending web request"

  return . (client api) $ mkAuthenticatedRequest auth \_ath -> authReq

infixl 1 `withPayload`
withPayload :: Functor f => f (a -> b) -> a -> f b
clientFun `withPayload` arg = (\f -> f arg) <$> clientFun

