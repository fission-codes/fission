-- | Continuous file sync
module Fission.CLI.Watch (command, watcher) where

import           RIO
import           RIO.State
import qualified RIO.Text as Text

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import           System.FSNotify as FS
import           Turtle                     hiding (err, (<&>))

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

-- import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji
import qualified Fission.Web.Client      as Client

import           Fission.CLI.Up (up)
import           Fission.CLI.Loader
import           Fission.CLI.Types

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
        => m ()
watcher = do
  cfg <- ask
  dir <- encodeString <$> pwd
  putText $ Emoji.eyes <> " Watching " <> Text.pack dir <> " for changes...\n"

  up

  liftIO $ FS.withManager \watchMgr -> evalStateT (return "") do
    lastCID <- get
    FS.watchTree watchMgr dir (const True) . const $ runRIO cfg up
    forever $ threadDelay 100000 -- Sleep until interupted by user
