module Fission.CLI.IPFS.Daemon
  ( start
  , stop
  , module Fission.CLI.IPFS.Daemon.Class
  ) where

import           Fission.Prelude

import           Fission.CLI.IPFS.Daemon.Class

start ::
  ( MonadIO         m
  , MonadIPFSDaemon m
  )
  => m (Process () () ())
start = do
  process <- runDaemon
  waitForStartup
  return process
  where
    waitForStartup = do
      checkRunning >>= \case
        True  ->
          return ()

        False -> do
          threadDelay 1_000_000
          waitForStartup

-- NOTE gets called automatically in CLI.hs
stop :: MonadIO m => (Process () () ()) -> m ()
stop  = liftIO . stopProcess
