{-# LANGUAGE AllowAmbiguousTypes #-}

module Fission.Web.Client
  ( sendRequestM
  , sendAuthedRequest
  , withPayload
  , attachAuth -- authClient
  , ClientError (..)
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import qualified Crypto.PubKey.Ed25519                             as Ed25519

import           Servant.Client
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID

import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class
import           Fission.Web.Client.JWT

import           Fission.Web.Client.Internal.Orphanage.ClientError ()

sendRequestM ::
  ( MonadWebClient m
  , MonadRaise     m
  , m `Raises` ClientError
  )
  => m (ClientM a)
  -> m a
sendRequestM clientAction = ensureM (sendRequest =<< clientAction)

sendAuthedRequest ::
  ( MonadIO        m
  , MonadTime      m
  , MonadWebClient m
  , ServerDID      m
  , MonadWebAuth   m (AuthClientData auth)
  , MonadWebAuth   m Ed25519.SecretKey
  , MonadRaise     m
  , m `Raises` ClientError
  )
  => (AuthenticatedRequest auth -> ClientM a)
  -> m a
sendAuthedRequest req = do
  auth <- attachAuth
  ensureM $ sendRequest $ req auth

attachAuth ::
  ( MonadIO      m
  , MonadTime    m
  , ServerDID    m
  , MonadWebAuth m (AuthClientData auth)
  , MonadWebAuth m Ed25519.SecretKey
  )
  => m (AuthenticatedRequest auth) -- (Client ClientM api)
attachAuth = do
  auth    <- getAuth
  authReq <- mkAuthReq
  return $ mkAuthenticatedRequest auth \_ath -> authReq

infixl 1 `withPayload`
withPayload :: Functor f => f (a -> b) -> a -> f b
clientFun `withPayload` arg = (\f -> f arg) <$> clientFun

