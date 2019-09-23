-- | File sync, IPFS-style
module Fission.CLI.Up (command, up) where

import           RIO
import           RIO.ByteString
import qualified RIO.Text as Text

import           SuperRecord
import           Options.Applicative.Simple (addCommand)
import qualified System.Console.ANSI as ANSI
import           Turtle hiding ((<&>), err)

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Web.Client      as Client
import qualified Fission.Web.IPFS.Client as Fission
import           Fission.IPFS.CID.Types
import qualified Fission.Emoji           as Emoji

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Loader
import           Fission.CLI.Types

-- | The command to attach to the CLI tree
-- command :: MonadIO m => Config -> CommandM (m ())
command :: MonadRIO           (Rec cfg) m
        => HasLogFunc         (Rec cfg)
        => Has "fissionAPI" cfg Client.Runner
        => (Rec cfg)
        -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your current working directory up"
    (const $ runRIO cfg up)
    noop

-- | Sync the current working directory to the server over IPFS
up :: MonadRIO           (Rec cfg) m
   => HasLogFunc         (Rec cfg)
   => Has "fissionAPI" cfg Client.Runner
   => m ()
up = do
  logDebug "Starting single pin"
  addCurrentDir >>= \case
    Left bad ->
      logError $ display bad

    Right out -> do
      hash      <- Text.stripEnd <$> strict out
      authOrErr <- Auth.get
      case authOrErr of
        Left err -> do
          logError $ displayShow err
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
          putText $ Emoji.prohibited <> " Unable to read credentials. Try logging in with "
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
          putStr "fission-cli login"

        Right auth -> do
          Client.Runner runner <- asksR #fissionAPI
          let cid = CID hash
          logDebug $ "Pinning " <> displayShow cid

          res <- liftIO . withLoader 5000
                       . runner
                       . Fission.pin (Fission.request auth)
                       $ CID hash

          case res of
            Right _ -> do
              putText $ Emoji.rocket <> " Your current working directory is now live"

            Left err -> do
              logError $ displayShow err
              liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
              putText $ mconcat
                [ Emoji.prohibited
                , " Something went wrong. Please try again or file a bug report with "
                , "Fission support at https://github.com/fission-suite/web-api/issues/new"
                ]

          return ()

-- | Add the current working directory to IPFS locally
addCurrentDir :: MonadIO m => m (Either Text (Shell Line))
addCurrentDir = do
  dir <- pwd
  return $ case toText dir of
    Right txt -> Right . inproc "ipfs" ["add", "-q"] . pure $ unsafeTextToLine txt
    Left  bad -> Left bad
