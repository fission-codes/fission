module Fission.Web.Auth.Token.Basic.Class (MonadBasicAuth (..)) where

import Servant

import Fission.Prelude

class Monad m => MonadBasicAuth who m where
  -- | Check that some entity is authenticated and authorized
  getVerifier :: m (BasicAuthCheck who)
