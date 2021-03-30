module Fission.CLI.IPFS.Daemon
  ( stop
  , forceStop
  , module Fission.CLI.IPFS.Daemon.Class
  ) where

import           Fission.Prelude

import           Fission.CLI.IPFS.Daemon.Class

-- NOTE gets called automatically in CLI.hs
stop ::
  ( MonadIO     m
  , MonadLogger m
  )
  => Process () () ()
  -> m ()
stop daemonProc = do
  logDebug @Text "😈⏹️  Stopping IPFS Daemon"
  void forceStop
  liftIO $ stopProcess daemonProc

forceStop :: MonadIO m => m ExitCode
forceStop = runProcess . fromString $ "2>/dev/null 1>/dev/null killall fission-ipfs"
