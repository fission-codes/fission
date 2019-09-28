-- | Continuous file sync
module Fission.CLI.Watch (command, watcher) where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text
import           RIO.Time

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI
import           System.FSNotify as FS

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji
import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types

import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.IPFS.Peer    as IPFS.Peer
import qualified Fission.IPFS.Types   as IPFS

import           Fission.CLI.Up hiding (command)
import           Fission.CLI.Loader
import           Fission.CLI.Types

import qualified Fission.CLI.Auth as Auth

-- | The command to attach to the CLI tree
command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the IPFS network"
    (const $ runRIO cfg watcher)
    noop

-- | Continuously sync the current working directory to the server over IPFS
watcher :: MonadRIO          cfg m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => m ()
watcher = do
  dir <- getCurrentDirectory
  Client.Runner runner <- Config.get

  putText $ Emoji.eyes <> " Watching " <> Text.pack dir <> " for changes...\n"

  IPFS.addDir dir >>= \case
    Left err ->
      logError $ displayShow err

    Right initCID ->
      Auth.withAuth \auth ->
      up' auth runner initCID >>= \case
        Left err ->
          logError $ displayShow err -- "Filaed" -- FIXME

        Right (CID hash) -> liftIO $ FS.withManager \watchMgr -> do
          cfg       <- ask
          hashCache <- newMVar hash
          timeCache <- newMVar =<< getCurrentTime
          handleTreeChanges timeCache hashCache watchMgr cfg dir
          forever $ liftIO $ threadDelay 1000000 -- Sleep main thread

dohertyDiffTime :: NominalDiffTime
dohertyDiffTime = 0.4

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000

handleTreeChanges :: MVar UTCTime
                  -> MVar Text
                  -> WatchManager
                  -> cfg
                  -> FilePath
                  -> IO StopListening
handleTreeChanges timeCache hashCache watchMgr cfg dir =
  FS.watchTree watchMgr dir (const True) . const $ runRIO cfg do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    if diffUTCTime now oldTime < dohertyDiffTime
      then do
        logDebug "Fired within change lock window"
        return ()

      else do
        void $ swapMVar timeCache now
        threadDelay dohertyMicroSeconds -- Wait for all events to fire in sliding window
        addDir rawDir \cid@(CID newHash) -> do
          logDebug $ "New CID: " <> display newHash

          oldHash <- swapMVar hashCache newHash
          logDebug $ "Old CID: " <> display oldHash

          when (oldHash /= newHash) (void $ up' auth runner cid)
