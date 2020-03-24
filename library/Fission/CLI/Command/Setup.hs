-- | Setup command
module Fission.CLI.Command.Setup (command, setup) where

import           Fission.Prelude

import           Options.Applicative.Simple (addCommand)
import           Servant.API

import Servant.Client.Core
import           Network.HTTP.Types.Status

import qualified Fission.CLI.Display.Error       as CLI.Error
import qualified Fission.CLI.Display.Success     as CLI.Success
import qualified Fission.CLI.Prompt              as Prompt
import qualified Fission.CLI.Environment.Partial as Env.Partial

import qualified Fission.Internal.UTF8 as UTF8

import           Fission.Web.Client       as Client
import qualified Fission.Web.Client.User  as User.Client

import qualified Fission.User.Username.Types     as User
import qualified Fission.User.Email.Types        as User
import qualified Fission.User.Registration.Types as User

import qualified Fission.User.DID as DID
import           Fission.User.DID.Types

import           Fission.CLI.Config.Types
import           Fission.CLI.Config.Base

import qualified Fission.Key.Store as Key

-- | The command to attach to the CLI tree
command :: MonadIO m => BaseConfig -> CommandM (m ())
command cfg =
  addCommand
    "setup"
    "Setup Fission on your machine"
    (\_ -> runBase cfg setup)
    (pure ())

setup ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => m ()
setup = do
  doesExist <- Key.exists

  if doesExist
    then
      Client.run User.Client.verify >>= \case
        Right (User.Username username) ->
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

    else do
      createKey
      maybe createAccount upgradeAccount =<< Env.Partial.findBasicAuth

createAccount ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => m ()
createAccount = do
  username <- User.Username <$> Prompt.reaskNotEmpty' "Username: "
  email    <- User.Email    <$> Prompt.reaskNotEmpty' "Email: "

  Client.run (User.Client.register User.Registration {..}) >>= \case
    Right _ok ->
      CLI.Success.putOk "Registration successful!"

    Left err ->
      let
        errMsg = case err of
          FailureResponse _ (responseStatusCode -> status) ->
            if | status == status409        -> "It looks like that account already exists. Please pick another username or contact Fission support for account recovery."
               | statusIsClientError status -> "There was a problem with your request. Please try again or contact Fission support."
               | otherwise                  -> "There was a server error. Please try again or contact Fission support."

          ConnectionError _ -> "Trouble contacting the server. Please try again or contact Fission support."
          DecodeFailure _ _ -> "Trouble decoding the registration response. Please try again or contact Fission support."
          _                 -> "Invalid content type. Please try again or contact Fission support."

      in do
        CLI.Error.put err errMsg
        createAccount

upgradeAccount ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => BasicAuthData
  -> m ()
upgradeAccount auth = do
  shouldUpgrade <- Prompt.reaskYN <| mconcat
                [ "Upgrade account \""
                , decodeUtf8Lenient (basicAuthUsername auth)
                , "\"? (y/n) "
                ]

  when shouldUpgrade do
    createKey
    UTF8.putText "📝 Upgrading your account... "
    Key.publicKeyEd >>= \case
      Left err ->
        CLI.Error.put err "Could not read key file"

      Right pubkey ->
        pubkey
          |> DID.fromPubkey
          |> updateDID auth

createKey :: MonadIO m => m ()
createKey = do
  UTF8.putText "🔑 Creating your key at ~/.ssh/fission... "
  Key.forceCreate
  UTF8.putTextLn "done"

updateDID ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadWebClient m
  )
  => BasicAuthData
  -> DID
  -> m ()
updateDID auth did =
  did
    |> User.Client.updateDID auth
    |> Client.run
    |> bind \case
      Left err ->
        CLI.Error.put err "Could not upgrade account"

      Right _ok -> do
        _ <- Env.Partial.deleteHomeAuth
        CLI.Success.putOk "Upgrade successful!"
