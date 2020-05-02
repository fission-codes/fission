-- | Link to another account

module Fission.CLI.Command.Login where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Network.HTTP.Types.Status
import           Servant.Client.Core
import           Servant.API

import           Fission.Prelude
import qualified Fission.Internal.UTF8 as UTF8
import qualified Fission.Key           as Key

import           Fission.User.Username.Types
import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token
import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import           Fission.User.Registration.Types
import           Fission.User.Email.Types

import           Fission.CLI.Display.Error   as CLI.Error
import           Fission.CLI.Display.Success as CLI.Success

import qualified Fission.CLI.Prompt               as Prompt
import qualified Fission.CLI.Environment.Override as Env.Override

import           Fission.CLI.Command.Types

-- | The command to attach to the CLI tree
cmd ::
  ( MonadIO m
  , MonadLogger m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Command m () ()
cmd = Command
  { command     = "login"
  , description = "Connect this machine an existing account"
  , argParser   = pure ()
  , handler     = \_ -> login
  }

login = undefined
