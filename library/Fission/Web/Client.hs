module Fission.Web.Client
  ( sendRequestM
  , withPayload
  , authClient
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import           Servant.Client

import           Fission.Prelude
 
import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class



import           Servant.Client.Core
import Fission.Web.Client.JWT
import Fission.Authorization.ServerDID

import qualified Crypto.PubKey.Ed25519 as Ed25519

sendRequestM :: MonadWebClient m => m (ClientM a) -> m (Either ClientError a)
sendRequestM clientAction = sendRequest =<< clientAction

authClient ::
  ( MonadIO f
  , MonadTime f
  , ServerDID f
  , MonadWebAuth f (AuthClientData a)
  , MonadWebAuth f Ed25519.SecretKey
  , HasClient ClientM api
  , Client ClientM api ~ (AuthenticatedRequest a -> m b)
  )
  => Proxy api
  -> f (m b)
authClient pxy = do
  auth    <- getAuth
  authReq <- mkAuthReq
  return $ (client pxy) $ mkAuthenticatedRequest auth \_ath -> authReq

withPayload :: Functor f => f (a -> b) -> a -> f b
clientFun `withPayload` arg = (\f -> f arg) <$> clientFun
