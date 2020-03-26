-- | Login command
module Fission.CLI.Command.Logout (command, logout) where

import           Fission.Prelude
import           Data.Function

import           Options.Applicative.Simple (addCommand)

import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Config.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

-- | The command to attach to the CLI tree
command ::
  ( MonadUnliftIO   m
  , HasLogFunc cfg
  )
  => cfg
  -> CommandM (m ())
command cfg =
  addCommand
    "logout"
    "Logout of your Fission account"
    (const <| runRIO cfg logout)
    (pure ())

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
logout ::
  ( MonadReader cfg m
  , MonadLogger     m
  , MonadUnliftIO   m
  )
  => m ()
logout = do
  logDebugN "Starting logout sequence"

  possibleSuccess <- Environment.removeConfigFile

  case possibleSuccess of
    Right _ ->
      CLI.Success.putOk "You are now logged out of the Fission CLI!"

    Left err ->
      CLI.Error.put err "Oh no! we were unable to remove your ~/.fission.yaml file for some reason. Please try again."

