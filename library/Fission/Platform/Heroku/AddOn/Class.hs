module Fission.Platform.Heroku.AddOn.Class (MonadHerokuAddOn (..)) where

import           Servant
import           Fission.Prelude

class Monad m => MonadHerokuAddOn m where
  authorize :: m (BasicAuthCheck ByteString)
