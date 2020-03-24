-- | Whoami command
module Fission.CLI.Command.Whoami (command, whoami) where

import           Fission.Prelude

import           Options.Applicative.Simple (addCommand)

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import qualified Fission.User.Username.Types as User

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.Base

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Config.Connected.Error.Types as Error

-- | The command to attach to the CLI tree
command :: MonadIO m => BaseConfig -> CommandM (m ())
command cfg =
  addCommand
    "whoami"
    "Check the current user"
    (\_ -> runBase cfg whoami)
    (pure ())

whoami ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => m ()
whoami = 
  Key.exists >>= \case
    False -> do
      CLI.Error.notConnected Error.NoKeyFile
    True ->
      Client.run User.verify >>= \case
        Left err -> 
          CLI.Error.notConnected err
        Right (User.Username username) -> 
          CLI.Success.loggedInAs <| username
