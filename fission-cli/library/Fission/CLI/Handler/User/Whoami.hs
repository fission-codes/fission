-- | Whoami command
module Fission.CLI.Handler.User.Whoami (whoami) where

import qualified Data.Yaml                                 as YAML

import qualified Crypto.PubKey.Ed25519                     as Ed25519

import qualified RIO.Text                                  as Text

import           Network.HTTP.Types.Status
import           Servant.Client

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Authorization.ServerDID

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client                        as Client
import qualified Fission.Web.Client.User                   as User

import           Fission.CLI.Environment                   as Env
import           Fission.CLI.Environment.Path              as Path

import qualified Fission.CLI.Display.Error                 as CLI.Error
import qualified Fission.CLI.Display.Success               as CLI.Success

import           Fission.CLI.Environment                   as Env
import           Fission.CLI.WebNative.Mutation.Auth.Store as UCAN


-- | The command to attach to the CLI tree
whoami ::
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , UCAN.MonadStore  m
  , MonadWebClient   m
  , MonadEnvironment m
  , ServerDID        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , MoCnadCleanup     m
  , m `Raises` ClientError
  , m `Raises` YAML.ParseException
  , m `Raises` NotFound FilePath
  , Show    (OpenUnion (Errors m))
  , Display (OpenUnion (Errors m))
  , ClientError `IsMember` Errors m
  , Errors m `Contains` Errors m
  )
  => m ()
whoami = do
  proof <- getRootUserProof
  attempt (sendAuthedRequest proof User.whoami) >>= \case
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

      global <- Path.getGlobalPath
      UTF8.putTextLn $ "Please contact Fission support at https://fission.codes or delete " <> Text.pack global <> " and try again."
      raise err
