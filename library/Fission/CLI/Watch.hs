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

import           Fission.CLI.Up (up)
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
watcher = Auth.withAuth \auth -> do
  cfg <- ask
  Client.Runner runner <- Config.get
  rawDir <- pwd
  let dir = encodeString rawDir
  putText $ Emoji.eyes <> " Watching " <> Text.pack dir <> " for changes...\n"

  -- CID initialHash <- up
  -- hashCache       <- newMVar initialHash
  up
  hashCache       <- newMVar "NOTHING YET"
  timeCache       <- newMVar =<< getCurrentTime

  liftIO $ FS.withManager \watchMgr -> do
    void . FS.watchTree watchMgr dir (const True) . const $ runRIO cfg do
      now <- getCurrentTime
      logDebug $ "FIRED at " <> displayShow now

      oldTime <- readMVar timeCache

      if diffUTCTime now oldTime < 0.4 -- dohertyPicoSeconds
         then do
           logDebug "Fired within Dhority threshold"
           return ()
         else do
           void $ swapMVar timeCache now
           threadDelay dohertyMicroSeconds -- Wait for all events to fire in sliding window

           case toText rawDir of
             Left err -> logError $ displayShow err
             Right txtDir -> do
               rawOut  <- return $ inproc "ipfs" ["add", "-HQr", txtDir] (pure "")
               newHash <- Text.stripEnd <$> strict rawOut
               logDebug $ "New CID: " <> display newHash

               oldHash <- swapMVar hashCache newHash
               logDebug $ "Old CID: " <> display oldHash

               when (oldHash /= newHash) do
                 logDebug $ "Remote pinning " <> display newHash

                 liftIO (withLoader 50000 . runner . Fission.pin (Fission.request auth) $ CID newHash) >>= \case
                   Left err -> do
                     return $ inproc "ipfs" ["swarm", "connect", "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
                     liftIO (withLoader 50000 . runner . Fission.pin (Fission.request auth) $ CID newHash) >>= \case
                       Right _ -> do
                         putText $ Emoji.rocket <> "Your current working directory is now live\n"
                         putText $ Emoji.okHand <> newHash  <> "\n\n"

                       Left err -> do
                         logError $ displayShow err
                         liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
                         putText $ mconcat
                           [ Emoji.prohibited
                           , " Something went wrong. Please try again or file a bug report with "
                           , "Fission support at https://github.com/fission-suite/web-api/issues/new"
                           ]

                   Right _ -> do
                     putText $ Emoji.rocket <> "Your current working directory is now live\n"
                     putText $ Emoji.okHand <> newHash  <> "\n\n"

    forever $ liftIO $ threadDelay 1000000 -- Sleep until interupted by user

dohertyPicoSeconds :: NominalDiffTime
dohertyPicoSeconds = 400000000000

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000
