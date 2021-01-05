module Fission.Web.Server.Auth.Class (MonadAuth (..)) where

import           Network.Wai
import           Servant.Server.Experimental.Auth

import           Fission.Prelude

class Monad m => MonadAuth who m where
  -- | Check that some entity is authenticated and authorized
  getVerifier :: m (AuthHandler Request who)
