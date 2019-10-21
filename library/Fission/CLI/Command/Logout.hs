-- | Login command
module Fission.CLI.Command.Logout (command, logout) where

import           RIO

import           Options.Applicative.Simple (addCommand)

import           Fission.Internal.Constraint

import qualified Fission.CLI.Auth as Auth
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

-- | The command to attach to the CLI tree
command :: MonadIO m
        => HasLogFunc        cfg
        => cfg
        -> CommandM (m ())
command cfg =
  addCommand
    "logout"
    "Logout of your Fission account"
    (const $ runRIO cfg logout)
    (pure ())

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
logout :: MonadRIO          cfg m
      => MonadUnliftIO         m
      => HasLogFunc        cfg
      => m ()
logout = do
  logDebug "Starting logout sequence"

  possibleSuccess <- Auth.removeConfigFile

  case possibleSuccess of
    Right _ ->
      CLI.Success.putOk "You are now logged out of the Fission CLI!"

    Left err ->
      CLI.Error.put err "Oh no! we were unable to remove your ~/.fission.yaml file for some reason. Please try again."

