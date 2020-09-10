module Fission.CLI.IPFS.Ignore.Class (MonadIPFSIgnore (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

-- TODO move to ipfs package
class Monad m => MonadIPFSIgnore m where
  getIgnoredFiles :: m IPFS.Ignored
