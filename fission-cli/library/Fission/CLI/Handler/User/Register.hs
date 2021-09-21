-- | Setup command
module Fission.CLI.Handler.User.Register (register) where

import qualified Data.Yaml                                   as YAML

import qualified Crypto.PubKey.Ed25519                       as Ed25519
import           Crypto.Random

import           Network.DNS
import           Network.HTTP.Types.Status
import           Servant.Client

import           Fission.Prelude

import           Fission.Authorization.ServerDID
import           Fission.Error.Types
import           Fission.Key                                 as Key
import           Fission.User.Username.Types

import           Fission.Web.Auth.Token.JWT.Types
import           Fission.Web.Auth.Token.Types
import           Fission.Web.Client                          as Client

import           Fission.User.DID.Types                      as DID
import           Fission.User.Email.Types
import           Fission.User.Registration.Types
import qualified Fission.User.Username.Error                 as Username

import           Fission.CLI.Remote

import           Fission.CLI.Display.Error                   as CLI.Error
import           Fission.CLI.Display.Success                 as CLI.Success

import           Fission.CLI.Environment                     as Env
import           Fission.CLI.Key.Store                       as KeyStore
import qualified Fission.CLI.Prompt                          as Prompt

import qualified Fission.CLI.WebNative.FileSystem.Auth.Store as WNFS
import           Fission.CLI.WebNative.Mutation.Auth.Store   as UCAN

register ::
  ( MonadIO          m
  , MonadRemote      m
  , MonadLogger      m
  , MonadWebClient   m
  , UCAN.MonadStore  m
  , MonadEnvironment m
  , MonadTime        m
  , MonadRandom      m
  , ServerDID        m
  , WNFS.MonadStore  m
  , MonadWebAuth     m Token
  , MonadWebAuth     m (SecretKey SigningKey)

  , MonadCleanup     m
  , m `Raises` AlreadyExists Env
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` Key.Error
  , m `Raises` NotFound DID
  , m `Raises` NotFound FilePath
  , m `Raises` Username.Invalid
  , m `Raises` YAML.ParseException

  , ClientError `IsMember` Errors m
  , Show (OpenUnion (Errors m))
  , Contains (Errors m) (Errors m)
  )
  => Maybe Username
  -> Maybe Email
  -> m Username
register maybeUsername maybeEmail = do
  attempt getRootUserProof >>= \case
    Left _ ->
      createAccount maybeUsername maybeEmail

    Right proof ->
      attempt (sendAuthedRequest proof whoAmI) >>= \case
        Left _ ->
          createAccount maybeUsername maybeEmail

        Right username -> do
          CLI.Success.alreadyLoggedInAs username
          return username

createAccount ::
  ( MonadIO          m
  , MonadRemote      m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , ServerDID        m
  , MonadRandom      m
  , WNFS.MonadStore  m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey

  , MonadCleanup     m
  , m `Raises` AlreadyExists Env
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` Key.Error
  , m `Raises` NotFound DID
  , m `Raises` Username.Invalid
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
    Nothing    -> ensureM $ mkUsername <$> Prompt.reaskNotEmpty' "Username:"
    Just uname -> return uname

  email <- case maybeEmail of
    Nothing   -> Email <$> Prompt.reaskNotEmpty' "Email:"
    Just mail -> return mail

  exchangePK <- KeyStore.fetchPublic (Proxy @ExchangeKey)
  signingPK  <- KeyStore.fetchPublic (Proxy @SigningKey)
  _          <- WNFS.create (DID.Key $ Ed25519PublicKey signingPK) "/"

  let
    form = Registration
      { username
      , email
      , password   = Nothing
      , exchangePK = Just exchangePK
      }

  attempt (sendAuthedRequest RootCredential $ createUser form) >>= \case
    Right _ok -> do
      CLI.Success.putOk "Registration successful! Head over to your email to confirm your account."
      baseURL <- getRemoteBaseUrl
      Env.init username baseURL Nothing
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

      CLI.Error.put err $ errMsg <> " Please try again or contact Fission support at https://fission.codes"
      raise err
