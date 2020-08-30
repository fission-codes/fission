-- | Whoami command
module Fission.CLI.Handler.Whoami (whoami) where

import qualified Crypto.PubKey.Ed25519           as Ed25519

import           Network.HTTP.Types.Status
import           Servant.Client.Core

import           Fission.Prelude

import qualified Fission.Internal.UTF8           as UTF8

import           Fission.Authorization.ServerDID
import qualified Fission.User.Username.Types     as User

import           Fission.Web.Auth.Token
import           Fission.Web.Client              as Client
import qualified Fission.Web.Client.User         as User

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success

-- | The command to attach to the CLI tree
whoami ::
  ( MonadIO        m
  , MonadTime      m
  , MonadLogger    m
  , MonadWebClient m
  , ServerDID      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  , MonadCleanup   m
  , m `Raises` ClientError
  , Show    (OpenUnion (Errors m))
  , Display (OpenUnion (Errors m))
  , IsMember ClientError (Errors m)
  )
  => m ()
whoami = do
  attempt (sendRequestM . authClient $ Proxy @User.WhoAmI) >>= \case
    Right User.Username {username} ->
      CLI.Success.currentlyLoggedInAs username

    Left err -> do
      CLI.Error.put err
        case openUnionMatch err of
          Nothing ->
            textDisplay err

          Just respErr ->
            case respErr of
              FailureResponse _ (responseStatusCode -> status) ->
                if | status == status404        -> "We don't recognize your key!"
                   | statusIsClientError status -> "There was a problem with your request."
                   | otherwise                  -> "There was a server error."

              ConnectionError _ -> "Trouble contacting the server."
              DecodeFailure _ _ -> "Trouble decoding the registration response."
              _                 -> "Invalid content type."

      UTF8.putText "Please contact Fission support at https://fission.codes or delete `~/.ssh/fission` and try again."
