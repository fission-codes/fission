-- | Login command
module Fission.CLI.Command.Logout (cmd, logout) where

import           Fission.Prelude

import qualified Fission.CLI.Environment as Environment
import           Fission.CLI.Command.Types

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

cmd :: (MonadUnliftIO m, MonadLogger m) => Command m () ()
cmd = Command
  { command     = "logout"
  , description = "Log out of your Fission account"
  , argParser   = pure ()
  , handler     = \_ -> logout
  }

-- | Login (i.e. save credentials to disk). Validates credentials agianst the server.
logout :: (MonadUnliftIO m, MonadLogger m) => m ()
logout = do
  logDebugN "Starting logout sequence"
  Environment.removeConfigFile >>= \case
    Right _  -> CLI.Success.putOk "You are now logged out of the Fission CLI!"
    Left err -> CLI.Error.put err "Oh no! we were unable to remove your ~/.fission.yaml file for some reason. Please try again."
