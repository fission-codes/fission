module Fission.Web.Auth.Class (MonadAuth (..)) where

import           Servant
import           Fission.Prelude

class Monad m => MonadAuth who m where
  verify :: m (BasicAuthCheck who)
