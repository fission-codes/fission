module Fission.CLI.IPFS.Daemon
  ( stop
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
  logDebug @Text "Stopping IPFS Daemon"
  runProcess . fromString $ "killall fission-ipfs"
  liftIO $ stopProcess daemonProc
