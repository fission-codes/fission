-- | Continuous file sync
module Fission.CLI.Command.Watch
  ( command
  , handleTreeChanges
  , watcher
  ) where

import           RIO
import           RIO.Directory
import           RIO.Process (HasProcessContext)
import qualified RIO.Text as Text
import           RIO.Time

import           Data.Has
import           System.FilePath

import           Options.Applicative.Simple (addCommand)
import           Options.Applicative (strArgument, metavar, help, value)
import           System.FSNotify as FS

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Web.Client   as Client
import qualified Fission.Storage.IPFS as IPFS
import qualified Fission.Time         as Time

import           Fission.IPFS.CID.Types
import qualified Fission.IPFS.Types as IPFS

import qualified Fission.CLI.Auth          as Auth
import           Fission.CLI.Config.Types
import qualified Fission.CLI.Display.Error as CLI.Error
import qualified Fission.CLI.Pin           as CLI.Pin

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the IPFS network"
    (runRIO cfg . watcher)
    (strArgument $ metavar "Location" <> help "The location of the assets you want to watch" <> value "./")

-- | Continuously sync the current working directory to the server over IPFS
watcher :: MonadRIO          cfg m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => HasProcessContext cfg
        => Has IPFS.BinPath  cfg
        => Has IPFS.Timeout  cfg
        => String
        -> m ()
watcher dir= do
  cfg <- ask
  curr <- getCurrentDirectory
  let dir' = if isAbsolute dir then dir else curr </> dir
  UTF8.putText $ "👀 Watching " <> Text.pack dir <> " for changes...\n"

  IPFS.addDir dir' >>= \case
    Left err ->
      CLI.Error.put' $ textDisplay err

    Right initCID ->
      Auth.withAuth $ CLI.Pin.run initCID >=> \case
        Left err ->
          CLI.Error.put' err

        Right (CID hash) -> liftIO $ FS.withManager \watchMgr -> do
          hashCache <- newMVar hash
          timeCache <- newMVar =<< getCurrentTime
          void $ handleTreeChanges timeCache hashCache watchMgr cfg dir'
          forever $ liftIO $ threadDelay 1000000 -- Sleep main thread

handleTreeChanges :: HasLogFunc        cfg
                  => Has Client.Runner cfg
                  => HasProcessContext cfg
                  => Has IPFS.BinPath  cfg
                  => Has IPFS.Timeout  cfg
                  => MVar UTCTime
                  -> MVar Text
                  -> WatchManager
                  -> cfg
                  -> FilePath
                  -> IO StopListening
handleTreeChanges timeCache hashCache watchMgr cfg dir =
  FS.watchTree watchMgr dir (const True) . const $ runRIO cfg do
    now     <- getCurrentTime
    oldTime <- readMVar timeCache

    if diffUTCTime now oldTime < Time.doherty
      then
        logDebug "Fired within change lock window"

      else do
        void $ swapMVar timeCache now
        threadDelay Time.dohertyMicroSeconds -- Wait for all events to fire in sliding window
        IPFS.addDir dir >>= \case
          Left err ->
            CLI.Error.put' err

          Right cid@(CID newHash) -> do
            oldHash <- swapMVar hashCache newHash
            logDebug $ "CID: " <> display oldHash <> " -> " <> display newHash
            when (oldHash /= newHash) $ Auth.withAuth (void . CLI.Pin.run cid)
