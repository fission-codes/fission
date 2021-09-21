module Fission.CLI.Handler.Setup (setup) where

import           Network.IPFS
import qualified Network.IPFS.Process.Error                  as IPFS.Process
import           RIO.FilePath

import           Network.DNS                                 as DNS
import           Servant.Client

import           Fission.Prelude

import           Fission.Error
import           Fission.Key                                 as Key

import           Fission.User.DID.Types                      as DID
import           Fission.User.Email.Types
import qualified Fission.User.Username.Error                 as Username
import           Fission.User.Username.Types

import           Fission.Web.Auth.Token.JWT.Types
import           Fission.Web.Client                          as Client

import           Fission.CLI.Display.Error                   as CLI.Error

import           Fission.CLI.Environment                     as Env
import qualified Fission.CLI.Environment.OS                  as OS

import           Fission.CLI.GitHub.Class                    as GitHub
import           Fission.CLI.Remote

import qualified Fission.CLI.User                            as User

import qualified Fission.CLI.Display.Success                 as Display
import qualified Fission.CLI.IPFS.Executable                 as Executable
import qualified Fission.CLI.Prompt                          as Prompt

import           Fission.CLI.Key.Store                       as Key
import           Fission.CLI.Key.Store                       as Key.Store

import qualified Fission.CLI.Handler.User.Login              as Login
import qualified Fission.CLI.Handler.User.Login              as User
import qualified Fission.CLI.Handler.User.Register           as User

import qualified Fission.CLI.WebNative.FileSystem.Auth.Store as WNFS

setup ::
  ( MonadIO          m
  , MonadLocalIPFS   m
  , MonadGitHub      m
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
  -> Maybe FilePath
  -> m ()
setup maybeOS maybeUsername maybeEmail maybeKeyFile = do
  logUser @Text "🌱 Setting up environment"
  Executable.place maybeOS

  attempt User.ensureNotLoggedIn >>= \case
    Left _ ->
      Display.putOk "Done! You're all ready to go 🚀"

    Right () -> do
      logUser @Text "🔑 Setting up keys"
      void . Key.Store.create $ Proxy @ExchangeKey
      case maybeKeyFile of
        Just keyFile -> do
          logDebug $ "🔑 Got a Keyfile: " <> keyFile
          Key.Store.fromFile (Proxy @SigningKey) keyFile

          attempt (sendAuthedRequest RootCredential whoAmI) >>= \case
            Left err -> do
              CLI.Error.put err "Invalid key file provided."
              raise err

            Right username -> do
              baseURL <- getRemoteBaseUrl
              signingPK  <- Key.Store.fetchPublic (Proxy @SigningKey)
              _ <- WNFS.create (DID.Key $ Ed25519PublicKey signingPK) "/"
              Env.init username baseURL Nothing
              Display.putOk $ "Done! Welcome to Fission, " <> textDisplay username <> " ✨"

        Nothing -> do
          void . Key.Store.create $ Proxy @SigningKey
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
