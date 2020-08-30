module Fission.App.URL.Class (HasAppURL (..)) where

import Fission.Prelude
import Fission.URL.Types

-- | Interface for getting the base app URL
class Monad m => HasAppURL m where
  -- | Get the base app URL (at time of writing: 'fission.app')
  getAppURL :: m URL
