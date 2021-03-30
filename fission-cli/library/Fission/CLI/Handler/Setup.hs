module Fission.CLI.Handler.Setup (setup) where

import           Network.IPFS
import qualified Network.IPFS.Process.Error        as IPFS.Process

import           Network.DNS                       as DNS
import           Servant.Client

import           Fission.Prelude

import           Fission.Error
import           Fission.Key.Error                 as Key

import           Fission.User.DID.Types
import           Fission.User.Email.Types
import qualified Fission.User.Username.Error       as Username
import           Fission.User.Username.Types

import           Fission.Web.Client                as Client
import           Fission.Web.Client.HTTP.Class

import qualified Fission.CLI.Environment.OS        as OS
import           Fission.CLI.Environment.Types
import           Fission.CLI.Remote

import qualified Fission.CLI.User                  as User

import qualified Fission.CLI.Display.Success       as Display
import qualified Fission.CLI.IPFS.Executable       as Executable
import qualified Fission.CLI.Prompt                as Prompt

import           Fission.CLI.Key.Store             as Key
import           Fission.CLI.Key.Store             as Key.Store

import qualified Fission.CLI.Handler.User.Login    as Login
import qualified Fission.CLI.Handler.User.Login    as User
import qualified Fission.CLI.Handler.User.Register as User

setup ::
  ( MonadIO          m
  , MonadLocalIPFS   m
  , MonadManagedHTTP m
  , MonadWebAuth     m (SecretKey SigningKey)

  , m `Raises` AlreadyExists DID
  , m `Raises` AlreadyExists Env
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` IPFS.Process.Error
  , m `Raises` Key.Error
  , m `Raises` NotFound DID
  , m `Raises` OS.Unsupported
  , m `Raises` Username.Invalid

  , CheckErrors m
  , ClientError `IsMember` Errors m

  , Show (OpenUnion (Errors m))

  , User.LoginConstraints m
  )
  => Maybe OS.Supported
  -> Maybe Username
  -> Maybe Email
  -> m ()
setup maybeOS maybeUsername maybeEmail = do
  logUser @Text "🌱 Setting up environment"
  Executable.place maybeOS

  attempt User.ensureNotLoggedIn >>= \case
    Left _ ->
      Display.putOk "Done! You're all ready to go 🚀"

    Right () -> do
      logUser @Text "🔑 Creating keys"
      void . Key.Store.create $ Proxy @SigningKey
      void . Key.Store.create $ Proxy @ExchangeKey

      username <- do
        Prompt.reaskYN "🏠 Do you have an existing account?" >>= \case
          False ->
            User.register maybeUsername maybeEmail

          True -> do
            logUser @Text "🔗 Please open auth.fission.codes on a signed-in device"
            signingSK <- Key.Store.fetch $ Proxy @SigningKey
            rootURL   <- getRemoteBaseUrl
            Login.consume signingSK rootURL {baseUrlPath = "/user/link"} maybeUsername

      Display.putOk $ "Done! Welcome to Fission, " <> textDisplay username <> " ✨"
