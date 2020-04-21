-- | Whoami command
module Fission.CLI.Command.Whoami (cmd, whoami) where

import           Fission.Prelude

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User

import qualified Fission.User.Username.Types as User
 
import           Fission.CLI.Command.Types

import qualified Fission.Web.Client.User as User

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Config.Connected.Error.Types as Error

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import Fission.Authorization.ServerDID
import Fission.Web.Auth.Token
import qualified Crypto.PubKey.Ed25519 as Ed25519


import qualified RIO.Text as Text

import           Network.HTTP.Types.Status
import           Servant.Client.Core
import           Servant.API

import           Fission.Prelude

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success
import qualified Fission.CLI.Prompt              as Prompt
import qualified Fission.CLI.Environment.Partial as Env.Partial

import           Fission.CLI.Command.Types

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Client      as Client
import qualified Fission.Web.Client.User as User.Client

import qualified Fission.Key  as Key
import qualified Fission.User as User


-- | The command to attach to the CLI tree
cmd ::
  ( MonadIO        m
  , MonadTime      m
  , MonadLogger    m
  , MonadWebClient m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
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
  , MonadTime      m
  , MonadLogger    m
  , MonadWebClient m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => m ()
whoami = 
  sendRequestM (authClient $ Proxy @User.WhoAmI) >>= \case
    Right User.Username {username} ->
      CLI.Success.loggedInAs username

    Left err ->
      let
        commonErrMsg = "Please contact Fission support or delete `~/.ssh/fission` and try again."
        specific = case err of
          FailureResponse _ (responseStatusCode -> status) ->
            if | status == status404        -> "We don't recognize your key!"
               | statusIsClientError status -> "There was a problem with your request."
               | otherwise                  -> "There was a server error."

          ConnectionError _ -> "Trouble contacting the server."
          DecodeFailure _ _ -> "Trouble decoding the registration response."
          _                 -> "Invalid content type."
      in
        CLI.Error.put err (specific <> " " <> commonErrMsg)
