-- | File sync, IPFS-style
module Fission.CLI.Up (command, up) where

import           RIO
import           RIO.ByteString
import qualified RIO.Text as Text

import           Data.Has
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI
import           Turtle hiding ((<&>), err)

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types
import qualified Fission.Config          as Config
import qualified Fission.Emoji           as Emoji

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Loader
import           Fission.CLI.Types

-- | The command to attach to the CLI tree
command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (const $ runRIO cfg up)
    noop

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO          cfg m
   => HasLogFunc        cfg
   => Has Client.Runner cfg
   => m ()
up = do
  logDebug "Starting single IPFS add locally"
  addCurrentDir >>= \case
    Left bad ->
      logError $ display bad

    Right out -> do
      hash   <- Text.stripEnd <$> strict out
      result <- Auth.get
      case result of
        Left err -> do
          logError $ displayShow err
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
          putText $ Emoji.prohibited <> " Unable to read credentials. Try logging in with "
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
          putStr "fission-cli login"

        Right auth -> do
          Client.Runner runner <- Config.get
          let cid = CID hash
          logDebug $ "Remote pinning " <> displayShow cid

          liftIO (withLoader 50000 . runner $ Fission.pin (Fission.request auth) cid) >>= \case
            Left err -> do
              logError $ displayShow err
              liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
              putText $ mconcat
                [ Emoji.prohibited
                , " Something went wrong. Please try again or file a bug report with "
                , "Fission support at https://github.com/fission-suite/web-api/issues/new"
                ]

            Right _ -> do
              putText $ "\n" <> Emoji.rocket <> "Your current working directory is now live"
              putText $ "\n" <> Emoji.okHand <> hash  <> "\n"

-- | Add the current working directory to IPFS locally
addCurrentDir :: MonadIO m => m (Either Text (Shell Line))
addCurrentDir = do
  dir <- pwd
  return $ case toText dir of
    Right txt -> Right $ inproc "ipfs" ["add", "-HQr", txt] (pure "")
    Left  bad -> Left bad
