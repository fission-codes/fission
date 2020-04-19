module Fission.Web.Client
  ( sendRequestM
  -- , sendAuthRequest
  -- , sendAuthRequest1
  , withPayload
  , authClient
  -- , authClient1
  -- , authClient2
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

-- import Servant.Client

-- import Fission.Web.Auth.Token.JWT
-- import Fission.Web.Auth.Token

-- import qualified Fission.Web.Auth.Token.Bearer as Token
import qualified Crypto.PubKey.Ed25519 as Ed25519

-- import Servant.Client.Core.Auth
-- import Fission.Web.Client.JWT

sendRequestM :: MonadWebClient m => m (ClientM a) -> m (Either ClientError a)
sendRequestM clientAction = sendRequest =<< clientAction

-- -- FIXME rename to withAuth, so can write sendRequest . withAuth $ client foo
-- -- or maybe sendRequest $ authClient1 foo arg
-- --          sendRequest $ authClient2 bar arg1 arg2
-- -- ACTUALLY shuldn't this just be `sendRequedst $ (authClient foo) arg1 arg2`
-- sendAuthRequest ::
--   ( MonadIO m
--   , MonadTime m
--   , ServerDID m
--   , MonadWebAuth m (AuthClientData a)
--   , MonadWebAuth m Ed25519.SecretKey
--   , MonadWebClient m
--   )
--   => (AuthenticatedRequest a -> ClientM b)
--   -> m (Either ClientError b)
-- sendAuthRequest endpoint = do
--   auth    <- getAuth
--   authReq <- mkAuthReq
--   sendRequest $ endpoint $ mkAuthenticatedRequest auth \_ath -> authReq

-- sendAuthRequest1 :: (MonadIO m, MonadTime m, ServerDID m,
--                            MonadWebAuth m (AuthClientData a1),
--                            MonadWebAuth m Ed25519.SecretKey, MonadWebClient m) =>
--                           (AuthenticatedRequest a1 -> t -> ClientM a2)
--                           -> t -> m (Either ClientError a2)
-- sendAuthRequest1 endpoint arg = do
--   auth    <- getAuth
--   authReq <- mkAuthReq
--   sendRequest $ endpoint (mkAuthenticatedRequest auth \_ath -> authReq) arg

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
clientFun `withPayload` arg = fmap ($ arg) clientFun

-- authClient1 :: (MonadIO f, MonadTime f, ServerDID f,
--                       MonadWebAuth f (AuthClientData a),
--                       MonadWebAuth f Ed25519.SecretKey, HasClient ClientM api,
--                       Client ClientM api ~ (AuthenticatedRequest a -> t -> b)) =>
--                      Proxy api -> t -> f b
-- authClient1 pxy arg  = do
--   auth    <- getAuth
--   authReq <- mkAuthReq
--   return $ (client pxy) (mkAuthenticatedRequest auth \_ath -> authReq) arg

-- authClient2 :: (MonadIO f, MonadTime f, ServerDID f,
--                       MonadWebAuth f (AuthClientData a),
--                       MonadWebAuth f Ed25519.SecretKey, HasClient ClientM api,
--                       Client ClientM api ~ (AuthenticatedRequest a -> t1 -> t2 -> b)) =>
--                      Proxy api -> t1 -> t2 -> f b
-- authClient2 pxy argA argB  = do
--   auth    <- getAuth
--   authReq <- mkAuthReq
--   return $ (client pxy) (mkAuthenticatedRequest auth \_ath -> authReq) argA argB
