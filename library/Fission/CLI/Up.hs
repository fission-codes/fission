module Fission.CLI.Up (command, up) where

import           RIO
import           RIO.ByteString

import           Data.Has
import           Options.Applicative.Simple (addCommand)

import Turtle

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Web.Client       as Client
import qualified Fission.Web.IPFS.Client  as Fission
import           Fission.IPFS.CID.Types
import qualified Fission.Config           as Config

import           Fission.CLI.Loader
import           Fission.CLI.Types

command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "up"
    "Keep your file up"
    (const $ runRIO cfg up)
    noop

up :: MonadRIO         cfg m
   => MonadUnliftIO        m
   => HasLogFunc       cfg
   => Has Client.Runner cfg
   => m ()
up = do
  logDebug "Starting single pin"
  addCurrentDir >>= \case
    Left bad ->
      logError $ display bad

    Right out -> do
      hash <- strict out
      logDebug $ display hash

      Client.Runner runner <- Config.get
      res <- liftIO . withLoader 5000 . runner $ Fission.pin $ CID hash
      case res of
        Right _ -> do
          logDebug "YEP"
        Left err -> do
          logError $ displayShow err

      -- auth <- Auth.get
      return ()

addCurrentDir :: MonadIO m => m (Either Text (Shell Line))
addCurrentDir = do
  dir <- pwd
  return $ case toText dir of
    Right txt -> Right . inproc "ipfs" ["add", "-q"] . pure $ unsafeTextToLine txt
    Left  bad -> Left bad
