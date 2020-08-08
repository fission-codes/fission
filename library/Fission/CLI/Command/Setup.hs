-- | Setup command
module Fission.CLI.Command.Setup (cmd, setup) where

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

import qualified Fission.User.Username.Types     as User
import           Fission.User.Registration.Types
import           Fission.User.Email.Types
 
import           Fission.CLI.Display.Error   as CLI.Error
import           Fission.CLI.Display.Success as CLI.Success

import qualified Fission.CLI.Prompt               as Prompt
import qualified Fission.CLI.Environment          as Env
import qualified Fission.CLI.Environment.Override as Env.Override

import           Fission.CLI.Command.Types

-- | The command to attach to the CLI tree
cmd ::
  ( MonadUnliftIO m
  , MonadLogger m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Command m () ()
cmd = Command
  { command     = "setup"
  , description = "Setup Fission on your machine"
  , argParser   = pure ()
  , handler     = \_ -> setup
  }

setup ::
  ( MonadUnliftIO m
  , MonadLogger m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  ) => m ()
setup =
  sendRequestM (authClient $ Proxy @User.WhoAmI) >>= \case
    Right User.Username {username} ->
      CLI.Success.alreadyLoggedInAs username

    Left _ ->
      maybe createAccount upgradeAccount =<< Env.Override.findBasicAuth

createAccount ::
  ( MonadUnliftIO m
  , MonadLogger m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  ) => m ()
createAccount = do
  username <- Username <$> Prompt.reaskNotEmpty' "Username: "
  email    <- Email    <$> Prompt.reaskNotEmpty' "Email: "
 
  let
    form = Registration
      { username = username
      , email    = email
      , password = Nothing
      }

  sendRequestM (authClient (Proxy @User.Register) `withPayload` form) >>= \case
    Right _ok -> do
      Env.init
      CLI.Success.putOk "Registration successful! Head over to your email to confirm your account."

    Left err ->
      let
        errMsg = case err of
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

      in do
        CLI.Error.put err $
          errMsg <> " Please try again or contact Fission support https://fission.codes/support."
         
        createAccount

upgradeAccount ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => BasicAuthData
  -> m ()
upgradeAccount auth = do
  shouldUpgrade <- Prompt.reaskYN $ mconcat
    [ "Upgrade account \""
    , decodeUtf8Lenient (basicAuthUsername auth)
    , "\"? (Y/n) "
    ]

  when shouldUpgrade do
    createKey
    UTF8.putText "📝 Upgrading your account... "
    Key.publicKeyEd >>= \case
      Left  err -> CLI.Error.put err "Could not read key file"
      Right pk  -> updateDID $ Key.Ed25519PublicKey pk

createKey :: MonadIO m => m ()
createKey = do
  UTF8.putText "🔑 Creating your key at ~/.ssh/fission... "
  Key.forceCreate
  UTF8.putTextLn "done"

updateDID ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  , MonadTime      m
  , ServerDID      m
  , MonadWebAuth   m Token
  , MonadWebAuth   m Ed25519.SecretKey
  )
  => Key.Public
  -> m ()
updateDID pk = do
  sendRequestM (authClient (Proxy @User.UpdatePK) `withPayload` pk) >>= \case
    Left err ->
      CLI.Error.put err "Could not upgrade account"

    Right _ -> do
      _ <- Env.Override.deleteHomeAuth
      CLI.Success.putOk "Upgrade successful!"
