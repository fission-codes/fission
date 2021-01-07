-- | Setup command
module Fission.CLI.Handler.User.Register (register) where

import qualified Crypto.PubKey.Ed25519           as Ed25519
import           Crypto.Random

import           Network.HTTP.Types.Status
import           Servant.Client

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Key.Error               as Key
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
import           Fission.CLI.Key.Store           as KeyStore
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
  , MonadWebAuth     m (SecretKey SigningKey)

  , MonadCleanup     m
  , m `Raises` ClientError
  , m `Raises` Username.Invalid
  , m `Raises` Key.Error

  , ClientError `IsMember` Errors m
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  )
  => Maybe Username
  -> Maybe Email
  -> m Username
register maybeUsername maybeEmail =
  attempt (sendAuthedRequest User.whoami) >>= \case
    Right username -> do
      CLI.Success.alreadyLoggedInAs username
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
  , m `Raises` ClientError
  , m `Raises` Username.Invalid
  , m `Raises` Key.Error
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  , ClientError `IsMember` Errors m
  )
  => Maybe Username
  -> Maybe Email
  -> m Username
createAccount maybeUsername maybeEmail = do
  logDebug @Text "🆔 Setting up new account"

  username <- case maybeUsername of
    Nothing    -> ensureM $ mkUsername <$> Prompt.reaskNotEmpty' "Username: "
    Just uname -> return uname

  email <- case maybeEmail of
    Nothing   -> Email <$> Prompt.reaskNotEmpty' "Email: "
    Just mail -> return mail

  exchangeSK <- KeyStore.fetch $ Proxy @ExchangeKey
  exchangePK <- KeyStore.toPublic (Proxy @ExchangeKey) exchangeSK

  let
    form = Registration
      { username
      , email
      , password   = Nothing
      , exchangePK = Just exchangePK
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
