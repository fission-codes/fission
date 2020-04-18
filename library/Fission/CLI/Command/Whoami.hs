-- | Whoami command
module Fission.CLI.Command.Whoami (cmd, whoami) where

import           Fission.Prelude

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import qualified Fission.User.Username.Types as User
 
import           Fission.CLI.Command.Types

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Config.Connected.Error.Types as Error -- FIXME try tyo el;imiate

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error


-- | The command to attach to the CLI tree
cmd ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => Command m () ()
cmd =
  Command
    { command     = "whoami"
    , description = "Check the current user"
    , argParser   = pure ()
    , handler     = \_ -> whoami
    }

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
      Client.run User.whoami >>= \case
        Right (User.Username username) ->  CLI.Success.loggedInAs username
        Left err ->  CLI.Error.notConnected err
