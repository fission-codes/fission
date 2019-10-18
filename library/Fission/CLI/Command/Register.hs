-- | Register command
module Fission.CLI.Command.Register (command, register) where

import           RIO
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS
import           Data.Has
import qualified Data.Text as T

import           Options.Applicative.Simple (addCommand)
import           Servant
import           System.Console.Haskeline

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import qualified Fission.Web.User.Client  as User.Client
import qualified Fission.Web.Client.Types as Client

import qualified Fission.User.Registration.Types as User

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadUnliftIO m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "register"
    "Register for Fission and login"
    (const $ runRIO cfg register)
    (pure ())

-- | Register and login (i.e. save credentials to disk)
register :: MonadRIO       cfg m
        => MonadUnliftIO         m
        => HasLogFunc        cfg
        => Has Client.Runner cfg
        => m ()
register = Auth.get >>= \case
  Right _auth ->
    CLI.Success.putOk "Already registered. Remove your credentials at ~/.fission.yaml if you want to re-register"

  Left _err ->
    register'

register' :: MonadRIO cfg m
          => MonadUnliftIO         m
          => HasLogFunc        cfg
          => Has Client.Runner cfg
          => m ()
register' = do
  logDebug "Starting registration sequence"
  putStr "Username: "
  username <- getLine

  liftIO (runInputT defaultSettings $ getPassword (Just '•') "Password: ") >>= \case
    Nothing ->
      logError "Unable to read password"

    Just password -> do
      putStr "Email: "
      email <- getLine
      logDebug "Attempting registration"
      let
        email' = if BS.null email then Nothing else Just $ decodeUtf8Lenient email
        user = User.Registration
               (decodeUtf8Lenient username)
               (T.pack password)
               email'
      let auth = BasicAuthData username $ BS.pack password

      Client.Runner runner <- Config.get

      registerResult <- Cursor.withHidden
                 . liftIO
                 . CLI.Wait.waitFor "Registering..."
                 . runner
                 $ User.Client.register user

      case registerResult of
        Right _ok -> Auth.write auth >> CLI.Success.putOk "Registered & logged in. Your credentials are in ~/.fission.yaml"
        Left  err -> CLI.Error.put err "Authorization failed"

-- promptPassword :: MonadRIO   cfg m
--             => MonadUnliftIO m
--             => HasLogFunc cfg
--             => ByteString
--             -> m Text
-- promptPassword prompt = 
--   putStr "asdf"
--   liftIO (runInputT defaultSettings $ getPassword (Just '•') "Password: ") >>= \case
--   Nothing ->
--     logError "Unable to read password"
--   Just password -> password
