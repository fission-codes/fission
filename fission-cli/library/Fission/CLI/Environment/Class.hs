module Fission.CLI.Environment.Class (MonadEnvironment (..)) where

import           Fission.Prelude

-- | A monad representing access to the configurable environment
class Monad m => MonadEnvironment m where
  getGlobalPath :: m FilePath -- ^ Must be the absolute path
