module Fission.CLI.IPFS.Daemon.Class (MonadIPFSDaemon (..), RunIPFSDaemon (..)) where

import           Fission.Prelude

class Monad m => MonadIPFSDaemon m where
  runDaemon    :: m (Process () () ())
  checkRunning :: m Bool

data RunIPFSDaemon = RunIPFSDaemon
  deriving (Eq, Show)
