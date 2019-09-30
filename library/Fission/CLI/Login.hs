-- | Login command
module Fission.CLI.Login (command, login) where

import           RIO
import           RIO.ByteString

import qualified Data.ByteString.Char8 as BS
import           Data.Has

import           Options.Applicative.Simple (addCommand)
import           Servant
import qualified System.Console.ANSI as ANSI
import           System.Console.Haskeline

import           Fission.Internal.Constraint
import qualified Fission.Internal.UTF8 as UTF8

import qualified Fission.Emoji           as Emoji
import qualified Fission.Config          as Config

import           Fission.Web.Auth.Client as Fission.Auth
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Auth   as Auth
import qualified Fission.CLI.Cursor as Cursor
import           Fission.CLI.Loader
import           Fission.CLI.Types
import qualified Fission.CLI.Success as CLI.Success
import qualified Fission.CLI.Error   as CLI.Error
import qualified Fission.CLI.Wait    as CLI.Wait

-- | The command to attach to the CLI tree
command :: MonadIO m => Config -> CommandM (m ())
command cfg =
  addCommand
    "login"
    "Add your Fission credentials"
    (const $ runRIO cfg login)
    (pure ())

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
login :: MonadRIO          cfg m
      => MonadUnliftIO         m
      => HasLogFunc        cfg
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

      authResult <- Cursor.withHidden
                 . liftIO
                 . CLI.Wait.waitFor "Verifying your credentials"
                 . runner
                 $ Fission.Auth.verify auth

      case authResult of
        Right _ok -> Auth.write auth >> CLI.Success.putOk "Logged in"
        Left  err -> CLI.Error.put err "Authorization failed"
