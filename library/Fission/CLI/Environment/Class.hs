module Fission.CLI.Environment.Class (MonadEnvironment (..)) where

import Fission.Prelude

import qualified Network.IPFS.Types as IPFS
import           Control.Monad.Except

class Monad m => MonadEnvironment m where
  getIgnoredFiles :: m IPFS.Ignored

instance MonadEnvironment m => MonadEnvironment (ExceptT SomeException m) where
  getIgnoredFiles = lift getIgnoredFiles
