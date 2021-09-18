{-# LANGUAGE UndecidableInstances #-}

module Fission.Web.Server.Auth.Class
  ( MonadAuth       (..)
  , GetAuthVerifier (..)
  ) where

import           Network.Wai                            as Wai
import           Servant.Server.Experimental.Auth

import           Fission.Prelude

import           Fission.Internal.Mock                  as Effect
import           Fission.User.DID.Types

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Mock.Config         as Mock
import           Fission.Web.Server.Models

class Monad m => MonadAuth who m where
  -- | Check that some entity is authenticated and authorized
  getVerifier :: m (AuthHandler Request who)

data GetAuthVerifier a = GetAuthVerifier
  deriving (Eq, Show)

instance GetAuthVerifier DID `IsMember` effs => MonadAuth DID (Mock effs Mock.Config) where
  getVerifier = do
    Effect.log $ GetAuthVerifier @DID
    asks didVerifier

instance GetAuthVerifier (Entity User) `IsMember` effs => MonadAuth (Entity User) (Mock effs Mock.Config) where
  getVerifier = do
    Effect.log $ GetAuthVerifier @(Entity User)
    asks userVerifier

instance GetAuthVerifier Authorization `IsMember` effs => MonadAuth Authorization (Mock effs Mock.Config) where
  getVerifier = do
    Effect.log $ GetAuthVerifier @Authorization
    asks authVerifier
