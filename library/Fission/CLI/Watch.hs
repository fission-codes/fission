-- | Continuous file sync
module Fission.CLI.Watch (command, watcher) where

import           RIO
import           RIO.ByteString
import qualified RIO.Text       as Text

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI        as ANSI
import           System.FSNotify
import           Turtle                     hiding (err, (<&>))

import Fission.Internal.Applicative
import Fission.Internal.Constraint

import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji
import           Fission.IPFS.CID.Types
import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission

import           Fission.CLI.Up (up)
import           Fission.CLI.Loader
import           Fission.CLI.Types

-- | The command to attach to the CLI tree
command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "watch"
    "Keep your working directory in sync with the network"
    (const $ runRIO cfg watcher)
    noop

-- | Continuously sync the current working directory to the server over IPFS
watcher :: MonadRIO          cfg m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => Has WatchManager  cfg
        => m ()
watcher = do
  logDebug "Starting single pin"
  mgr <- Config.get
  cfg <- ask
  dir <- encodeString <$> pwd
  putText $ Emoji.eyes <> " Watching " <> Text.pack dir <> " for changes..."

  up

  liftIO $ watchTree mgr dir (const True) . const $ runRIO cfg do
    putStr "\n"
    up

  forever $ threadDelay 10000
    -- case event of
    --   Added   filePath time bool ->
    --   Modified filePath time bool ->
    --   Removed filePath time bool ->
    --   Unknown filePath time bool ->
