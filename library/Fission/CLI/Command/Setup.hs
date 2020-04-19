-- | Setup command
module Fission.CLI.Command.Setup (cmd, setup) where

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
import qualified Fission.Web.Client.User as User

import qualified Fission.Key  as Key
-- import qualified Fission.User as User

import Servant.Client
import qualified Fission.CLI.Command.Whoami as WhoAmI

import           Network.IPFS.CID.Types
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client
import qualified Fission.Web.Client.DNS  as DNS

import           Fission.CLI.Display.Error   as CLI.Error
import qualified Fission.CLI.Display.Loader  as CLI
import           Fission.CLI.Display.Success as CLI.Success

import           Fission.URL.DomainName.Types as URL

import           Fission.Prelude

import           Fission.Web.Client      as Client
--import qualified Fission.Web.Client.User as User

import Fission.User.Registration.Types
import Fission.User.Email.Types

import           Fission.User.Username.Types

import           Fission.CLI.Command.Types

import qualified Fission.Key.Store as Key
import qualified Fission.CLI.Config.Connected.Error.Types as Error

import qualified Fission.CLI.Display.Success as CLI.Success
import qualified Fission.CLI.Display.Error   as CLI.Error

import Fission.Authorization.ServerDID
import Fission.Web.Auth.Token
import qualified Crypto.PubKey.Ed25519 as Ed25519


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
  { command     = "setup"
  , description = "Setup Fission on your machine"
  , argParser   = pure ()
  , handler     = \_ -> setup
  }

setup ::
  ( MonadIO m
  , MonadLogger m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  ) => m ()
setup = do
  Key.exists >>= \case
    True ->
      CLI.Success.putOk "You are already connected"

    False -> do
      createKey -- FIXME aboiut to be part of an ensure step on startup
      maybe createAccount upgradeAccount =<< Env.Partial.findBasicAuth

createAccount ::
  ( MonadIO m
  , MonadTime m
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

  sendRequestM (authClient User.register `withPayload` form) >>= \case
    Right _ok ->
      CLI.Success.putOk "Registration successful!"

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
          errMsg <> " Please try again or contact Fission support."
         
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
      Right pk  -> updateDID . Key.Public . encodeUtf8 . Text.pack $ show pk

createKey :: MonadIO m => m ()
createKey = do
  UTF8.putText "🔑 Creating your key at ~/.ssh/fission... "
  Key.forceCreate
  UTF8.putTextLn "done"

updateDID ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  , MonadTime m
  , ServerDID m
  , MonadWebAuth m Token
  , MonadWebAuth m Ed25519.SecretKey
  )
  => Key.Public
  -> m ()
updateDID pk = do
  sendRequestM (authClient User.updatePublicKey `withPayload` (pk, Key.Ed25519)) >>= \case
    Left err ->
      CLI.Error.put err "Could not upgrade account"

    Right _ -> do
      _ <- Env.Partial.deleteHomeAuth
      CLI.Success.putOk "Upgrade successful!"
