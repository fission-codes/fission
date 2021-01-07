module Fission.CLI.Remote.Class (MonadRemote (..)) where

import           Fission.Prelude

import           Fission.CLI.Remote.Types

class Monad m => MonadRemote m where
  getRemote :: m Remote
