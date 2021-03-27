{-# LANGUAGE AllowAmbiguousTypes #-}

module Fission.Web.Client
  ( sendRequestM
  , sendAuthedRequest
  , attachAuth -- authClient
  , ClientError (..)
  , module Fission.Web.Client.Auth
  , module Fission.Web.Client.Class
  ) where

import qualified Crypto.PubKey.Ed25519                  as Ed25519

import           Servant.Client
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Web.Auth.Token.JWT             as JWT

import           Fission.Web.Client.Auth
import           Fission.Web.Client.Class
import           Fission.Web.Client.JWT

import           Fission.Internal.Orphanage.ClientError ()

sendRequestM ::
  ( MonadWebClient m
  , MonadRaise     m
  , m `Raises` ClientError
  )
  => m (ClientM a)
  -> m a
sendRequestM clientAction = ensureM (sendRequest =<< clientAction)

sendAuthedRequest ::
  ( MonadTime      m
  , MonadWebClient m
  , ServerDID      m
  , MonadWebAuth   m (AuthClientData auth)
  , MonadWebAuth   m Ed25519.SecretKey
  , MonadRaise     m
  , m `Raises` ClientError
  )
  => JWT.Proof
  -> (AuthenticatedRequest auth -> ClientM a)
  -> m a
sendAuthedRequest proof req = do
  auth <- attachAuth proof
  ensureM . sendRequest $ req auth

attachAuth ::
  ( MonadTime    m
  , ServerDID    m
  , MonadWebAuth m (AuthClientData auth)
  , MonadWebAuth m Ed25519.SecretKey
  )
  => JWT.Proof
  -> m (AuthenticatedRequest auth) -- (Client ClientM api)
attachAuth proof = do
  auth    <- getAuth
  authReq <- mkAuthReq proof
  return $ mkAuthenticatedRequest auth \_ath -> authReq
