-- | Login command
module Fission.CLI.Login (command, login) where

import           RIO
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS
import           SuperRecord

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

-- | The command to attach to the CLI tree
command :: MonadRIO           (Rec cfg) m
        => HasLogFunc         (Rec cfg)
        => Has "fissionAPI" cfg Client.Runner
        => (Rec cfg)
        -> CommandM (m ())
command cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runRIO cfg login)
    noop

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
login :: MonadRIO   (Rec cfg) m
      => MonadUnliftIO        m
      => HasLogFunc       (Rec cfg)
      => Has "fissionAPI" cfg Client.Runner
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
      Client.Runner runner <- asksR #fissionAPI
      let auth = BasicAuthData username $ BS.pack password

      authResult <- Cursor.withHidden $ liftIO do
        ANSI.cursorForward 3
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
        putStr "Verifying your credentials..."
        ANSI.setCursorColumn 0
        withLoader 5000 . runner $ Fission.Auth.verify auth

      case authResult of
        Right _ok -> do
          Auth.set auth
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
          putText $ Emoji.whiteHeavyCheckMark <> " Logged in successfully!"

        Left err -> do
          logDebug $ displayShow err
          liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
          putText $ Emoji.prohibited <> " Authorization failed"
