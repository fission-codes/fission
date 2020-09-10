module Fission.CLI.Environment.Class (MonadEnvironment (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

-- | A monad representing access to a Fission server
class Monad m => MonadEnvironment m where
  getGlobalPath   :: m FilePath
  getIgnoredFiles :: m IPFS.Ignored
