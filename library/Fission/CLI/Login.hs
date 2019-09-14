module Fission.CLI.Login (command, login) where

import           RIO
import           RIO.ByteString
import           RIO.File

import qualified Data.ByteString.Char8 as BS
import           Data.Has
import           Data.Aeson
import qualified Data.Yaml as Yaml

import           Options.Applicative.Simple (addCommand)
import           Servant
import qualified System.Console.ANSI as ANSI
import           System.Console.Haskeline

import           Fission.Internal.Applicative
import           Fission.Internal.Constraint

import qualified Fission.Emoji           as Emoji
import qualified Fission.Config          as Config

import           Fission.Web.Auth.Client as Fission.Auth
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Auth   as Auth
import qualified Fission.CLI.Cursor as Cursor
import           Fission.CLI.Loader
import           Fission.CLI.Types

command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runRIO cfg login)
    noop

login :: MonadRIO         cfg m
      => MonadUnliftIO        m
      => HasLogFunc       cfg
      => Has Client.Runner cfg
      => m ()
login = do
  logDebug "Starting login sequence"
  putStr "Username: "
  username <- getLine
  liftIO (runInputT defaultSettings $ getPassword (Just '•') "Password: ") >>= \case
    Nothing ->
      logError "Unable to read password"

    Just password -> do
      logDebug "Attempting API verification"
      Client.Runner runner <- Config.get
      let auth = BasicAuthData username $ BS.pack password

      authResult <- Cursor.withHidden $ liftIO do
        ANSI.cursorForward 3
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
        putStr "Verifying your credentials..."
        ANSI.setCursorColumn 0
        withLoader 5000 . runner $ Fission.Auth.verify auth

      case authResult of
        Right _ok -> success auth
        Left  err -> failure err

success :: (MonadUnliftIO m, ToJSON a) => a -> m ()
success auth = do
  path <- Auth.getCachePath
  writeBinaryFileDurable path $ Yaml.encode auth

  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  putText $ Emoji.whiteHeavyCheckMark <> " Logged in successfully!"

failure :: (MonadRIO cfg m, HasLogFunc cfg, Show a) => a -> m ()
failure err = do
  logDebug $ displayShow err
  liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  putText $ Emoji.prohibited <> " Authorization failed"
