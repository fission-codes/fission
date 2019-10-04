-- | Register command
module Fission.CLI.Command.Register (command, register) where

import           RIO

import           Data.Has

import           Options.Applicative.Simple (addCommand)
import           Servant

import qualified Fission.Config as Config
import           Fission.Internal.Constraint

import qualified Fission.Web.Auth.Client  as Fission.Auth
import qualified Fission.Web.Client.Types as Client

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Cursor  as Cursor
import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Wait    as CLI.Wait

import qualified Fission.Platform.User  as User
import qualified Fission.Security.Types as Security

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
register :: MonadRIO          cfg m
      => MonadUnliftIO         m
      => HasLogFunc        cfg
      => Has Client.Runner cfg
      => m ()
register = do
  logDebug "Starting registration sequence"
  Client.Runner runner <- Config.get

  registerResult <- Cursor.withHidden
                  . liftIO
                  . CLI.Wait.waitFor "Registering"
                  . runner
                  $ Fission.Auth.register

  logDebug $ displayShow registerResult

  case registerResult of
    Right user -> do 
      logDebug $ displayShow user
      let username = encodeUtf8 $ user ^. User.interplanetaryFissionUsername
      let password = encodeUtf8 $ Security.unSecret $ user ^. User.interplanetaryFissionPassword
      let auth = BasicAuthData username password
      Auth.write auth >> CLI.Success.putOk "Registerd & logged in"
    Left  err -> CLI.Error.put err "Registeration failed"
