module Fission.CLI.IPFS.Daemon.Class (MonadIPFSDaemon (..)) where

import           Fission.Prelude

class Monad m => MonadIPFSDaemon m where
  runDaemon    :: m ()
  checkRunning :: m Bool
