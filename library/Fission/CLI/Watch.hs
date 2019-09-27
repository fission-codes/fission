-- | Continuous file sync
module Fission.CLI.Watch (command, watcher) where

import           RIO
import qualified RIO.Text as Text
import           RIO.Time

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI
import           System.FSNotify as FS
import           Turtle                     hiding (err, (<&>))

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji
import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types

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
        => m ()
watcher = do
  cfg    <- ask
  rawDir <- pwd
  let dir = encodeString rawDir
  putText $ Emoji.eyes <> " Watching " <> Text.pack dir <> " for changes...\n"

  Client.Runner runner <- Config.get
  Auth.withAuth \auth -> addDir rawDir \initCID -> up' auth runner initCID >>= \case
    Left err ->
      logError "Filaed" -- FIXME

    Right (CID hash) -> liftIO $ FS.withManager \watchMgr -> do
      hashCache <- newMVar hash
      timeCache <- newMVar =<< getCurrentTime

      void . FS.watchTree watchMgr dir (const True) . const $ runRIO cfg do
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

      forever $ liftIO $ threadDelay 1000000 -- Sleep until interupted by user

dohertyDiffTime :: NominalDiffTime
dohertyDiffTime = 0.4

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000
