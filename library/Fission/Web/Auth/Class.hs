module Fission.Web.Auth.Class (MonadAuth (..)) where

import           Servant.Server.Experimental.Auth
import           Network.Wai

import           Fission.Prelude

class Monad m => MonadAuth who m where
  -- | Check that some entity is authenticated and authorized
  getVerifier :: m (AuthHandler Request who)
