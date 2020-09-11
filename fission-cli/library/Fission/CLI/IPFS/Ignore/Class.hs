module Fission.CLI.IPFS.Ignore.Class (MonadIPFSIgnore (..)) where

import           Fission.Prelude

class Monad m => MonadIPFSIgnore m where
  getIgnoredFiles :: m [Text]
