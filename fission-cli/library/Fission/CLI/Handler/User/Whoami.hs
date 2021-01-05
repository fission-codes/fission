-- | Whoami command
module Fission.CLI.Handler.User.Whoami (whoami) where

import qualified Data.Yaml                       as YAML

import qualified Crypto.PubKey.Ed25519           as Ed25519

import           Network.HTTP.Types.Status
import           Servant.API                     hiding (IsMember)
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.API.User.Types

import           Fission.Error.NotFound.Types
import qualified Fission.Internal.UTF8           as UTF8

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client              as Client
import qualified Fission.Web.Client.User         as User

import           Fission.CLI.Environment         as Env

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success

-- | The command to attach to the CLI tree
whoami ::
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadWebClient   m
  , MonadEnvironment m
  , ServerDID        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  , Show    (OpenUnion (Errors m))
  , Display (OpenUnion (Errors m))
  , IsMember ClientError (Errors m)
  )
  => m ()
whoami = do
  attempt (sendAuthedRequest whoami') >>= \case
    Right username -> do
      CLI.Success.currentlyLoggedInAs $ textDisplay username
      Env.update \env -> env {username}

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

(createWithDID :<|> createWithPassword ) :<|> whoami' :<|> verify :<|> email :<|> did :<|> exchangeKeys :<|> dataRoot :<|> passwordReset = client $ Proxy @User
