-- | Setup command
module Fission.CLI.Handler.User.Register (register) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import           Crypto.Random

import           Network.DNS
import           Network.HTTP.Types.Status
import           Servant.Client

import           Fission.Prelude

import           Fission.Error
import qualified Fission.Key                     as Key

import           Fission.Authorization.ServerDID
import           Fission.User.DID.Types
import           Fission.User.Username.Types

import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client              as Client
import qualified Fission.Web.Client.User         as User

import           Fission.User.Email.Types
import           Fission.User.Registration.Types
import qualified Fission.User.Username.Error     as Username

import           Fission.CLI.Display.Error       as CLI.Error
import           Fission.CLI.Display.Success     as CLI.Success

import           Fission.CLI.Environment         as Env
import qualified Fission.CLI.Prompt              as Prompt

register ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadWebClient   m
  , MonadEnvironment m
  , MonadTime        m
  , MonadRandom      m
  , ServerDID        m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , m `Raises` AlreadyExists Ed25519.SecretKey
  , m `Raises` Username.Invalid
  , IsMember ClientError (Errors m)
  , IsMember Key.Error   (Errors m)
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  )
  => Maybe Username
  -> Maybe Email
  -> m Username
register maybeUsername maybeEmail = do
  attempt (sendAuthedRequest User.whoami) >>= \case
    Right username -> do
      CLI.Success.alreadyLoggedInAs $ textDisplay username
      return username

    Left _ ->
      createAccount maybeUsername maybeEmail

createAccount ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , ServerDID        m
  , MonadRandom      m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , MonadCleanup     m
  , IsMember ClientError (Errors m)
  , IsMember Key.Error   (Errors m)
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , m `Raises` AlreadyExists Ed25519.SecretKey
  , m `Raises` Username.Invalid
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  )
  => Maybe Username
  -> Maybe Email
  -> m Username
createAccount maybeUsername maybeEmail = do
  username <- case maybeUsername of
    Nothing    -> ensureM $ mkUsername <$> Prompt.reaskNotEmpty' "Username: "
    Just uname -> return uname

  email <- case maybeEmail of
    Nothing   -> Email <$> Prompt.reaskNotEmpty' "Email: "
    Just mail -> return mail

  let
    form = Registration
      { username
      , email
      , password = Nothing
      }

  attempt (sendAuthedRequest $ User.createWithDID form) >>= \case
    Right _ok -> do
      CLI.Success.putOk "Registration successful! Head over to your email to confirm your account."
      return username

    Left err -> do
      let
        errMsg =
          case openUnionMatch err of
            Nothing ->
              "Unknown Error"

            Just respErr ->
              case respErr of
                FailureResponse _ (responseStatusCode -> status) ->
                  if | status == status409 ->
                        "It looks like that account already exists."

                     | statusIsClientError status ->
                        "There was a problem with your request."

                     | otherwise ->
                        "There was a server error."

                ConnectionError _ ->
                  "Trouble contacting the server."

                DecodeFailure _ _ ->
                  "Trouble decoding the registration response."

                _ ->
                  "Invalid content type."

      CLI.Error.put err $
        errMsg <> " Please try again or contact Fission support at https://fission.codes"
      raise err
