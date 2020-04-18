-- | Login command
module Fission.CLI.Command.Logout (command, logout) where

import           Data.Function

import           Options.Applicative.Simple (addCommand)

import           Fission.Prelude
import           Fission.CLI.Command.Class

import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

command :: Command m () ()
command = Command
  { command     = "logout"
  , description = "Logout of your Fission account"
  , parseArgs   = pure ()
  , handler     = \_ -> logout
  , subCommands = []
  }

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
logout :: (MonadLogger m, MonadUpdateConfig m) => m ()
logout = do
  logDebugN "Starting logout sequence"

  possibleSuccess <- Environment.removeConfigFile

  case possibleSuccess of
    Right _ ->
      CLI.Success.putOk "You are now logged out of the Fission CLI!"

    Left err ->
      CLI.Error.put err "Oh no! we were unable to remove your ~/.fission.yaml file for some reason. Please try again."

