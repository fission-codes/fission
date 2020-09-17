module Fission.CLI.IPFS.Daemon
  ( start
  , stop
  , module Fission.CLI.IPFS.Daemon.Class
  ) where

import           Control.Concurrent            (forkIO, killThread)

import           Fission.Prelude

import           Fission.CLI.IPFS.Daemon.Class

start ::
  ( MonadIO         m
  , MonadIPFSDaemon m
  )
  => (m () -> IO ())
  -> m ThreadId
start inIO = do
  threadId <- liftIO . forkIO $ inIO runDaemon
  waitForStartup
  return threadId
  where
    waitForStartup = do
      checkRunning >>= \case
        True  ->
          return ()

        False -> do
          threadDelay 1_000_000
          waitForStartup

stop :: MonadIO m => ThreadId -> m ()
stop  = liftIO . killThread
