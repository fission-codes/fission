{-# LANGUAGE UndecidableInstances #-}

module Fission.Web.Server.Auth.Token.Basic.Class
  ( MonadBasicAuth (..)
  , GetBasicAuth   (..)
  ) where

import           Servant                           (BasicAuthCheck)

import           Fission.Prelude

import           Fission.Internal.Mock             as Effect
import qualified Fission.Web.API.Heroku.Auth.Types as Heroku
import           Fission.Web.Server.Mock.Config    as Mock

class Monad m => MonadBasicAuth who m where
  -- | Check that some entity is authenticated and authorized
  getVerifier :: m (BasicAuthCheck who)

data GetBasicAuth who = GetBasicAuth
  deriving (Eq, Show)

instance forall effs . GetBasicAuth Heroku.Auth `IsMember` effs => MonadBasicAuth Heroku.Auth (Mock effs Mock.Config) where
  getVerifier = do
    Effect.log $ GetBasicAuth @Heroku.Auth
    asks herokuVerifier
