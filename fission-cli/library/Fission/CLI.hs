module Fission.CLI (cli, interpret) where

import qualified Crypto.PubKey.Ed25519                          as Ed25519
import           Options.Applicative

import qualified RIO.Text                                       as Text

import           Network.HTTP.Client                            as HTTP
import           Network.HTTP.Client.TLS                        as HTTP
import           Servant.Client.Core

import qualified Network.IPFS.Process.Error                     as IPFS (Error)
import qualified Network.IPFS.Timeout.Types                     as IPFS
import qualified Network.IPFS.URL.Types                         as IPFS

import           Fission.Prelude

import           Fission.Error

import qualified Fission.CLI.IPFS.Daemon                        as IPFS.Daemon
import qualified Fission.CLI.Meta                               as Meta

import           Fission.User.DID.Types

import           Fission.CLI.Environment                        as Env
import qualified Fission.CLI.Environment.OS                     as OS

import qualified Fission.CLI.Base.Types                         as Base
import qualified Fission.CLI.Handler                            as Handler

import qualified Fission.CLI.Handler.Setup                      as Setup

import           Fission.CLI.Parser                             as CLI
import           Fission.CLI.Parser.Command.Setup.Types         as Setup
import           Fission.CLI.Parser.Command.Types
import           Fission.CLI.Parser.Command.User.Types
import           Fission.CLI.Parser.Types                       as Parser
import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.App                                as App
import           Fission.CLI.Types

import           Fission.Internal.Orphanage.Yaml.ParseException ()





import qualified RIO.ByteString                                 as BS

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                          as Ed25519
import qualified Crypto.PubKey.RSA.Types                        as RSA

import           Network.DNS                                    as DNS
import           Network.IPFS
import qualified Network.IPFS.Process.Error                     as IPFS.Process
import           Servant.Client

import           Fission.Error
import           Fission.Error.NotFound.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.ServerDID

import           Fission.User.DID.Types
import           Fission.User.Username.Types

import qualified Fission.Web.Auth.Token.JWT.Resolver.Error      as UCAN.Resolver
import           Fission.Web.Client.Class
import           Fission.Web.Client.HTTP.Class

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.JWT.Error               as JWT
import           Fission.Web.Client                             as Client

import           Fission.CLI.Environment                        as Env
import qualified Fission.CLI.Environment.OS                     as OS

import qualified Fission.CLI.Display.Success                    as Display
import qualified Fission.CLI.IPFS.Executable                    as Executable

import           Fission.CLI.Key.Store                          as Key

import qualified Fission.CLI.Handler.User.Link.Request          as Link
import qualified Fission.CLI.Handler.User.Register              as User

import qualified Fission.IPFS.PubSub.Session.Key.Types          as Session
import qualified Fission.IPFS.PubSub.Session.Payload            as Session
import           Fission.Web.Auth.Token.JWT                     as JWT
import qualified Fission.Web.Auth.Token.JWT                     as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error               as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class      as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error      as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation          as UCAN
import qualified Fission.Web.Auth.Token.UCAN                    as UCAN


type Errs
   = OS.Unsupported
  ': AlreadyExists Ed25519.SecretKey
  ': ClientError
  ': IPFS.Error
  -- Linking
  ': OS.Unsupported
  ': ClientError
  ': IPFS.Error
  ': Key.Error
  ': DNSError
  ': NotFound DID
  ': AlreadyExists Ed25519.SecretKey
  ': NotFound DID
  ': DNS.DNSError
  ': CryptoError
  ': IPFS.Process.Error
  ': String
  ': RSA.Error
  ': JWT.Error
  ': UCAN.Resolver.Error
  ': Key.Error
  -- App
  ': App.Errs




cli :: MonadUnliftIO m => m (Either (OpenUnion Errs) ())
cli = do
  Parser.Options {fissionDID, fissionURL, cmd} <- liftIO $ execParser CLI.parserWithInfo

  let
    VerboseFlag isVerbose = getter cmd
    Right fallbackDID = eitherDecode "\"did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4\""

    ipfsURL     = IPFS.URL $ BaseUrl Https "ipfs.io" 443 ""
    ipfsTimeout = IPFS.Timeout 3600

    rawHTTPSettings =
      case baseUrlScheme fissionURL of
        Http  -> defaultManagerSettings
        Https -> tlsManagerSettings

  httpManager <- liftIO $ HTTP.newManager rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro 1_800_000_000 }

  ipfsDaemonVar <- liftIO newEmptyMVar
  processCtx    <- mkDefaultProcessContext
  logOptions    <- logOptionsHandle stderr isVerbose

  withLogFunc logOptions \logFunc -> do
    finalizeDID fissionDID Base.Config {serverDID = fallbackDID, ..} >>= \case
      Right serverDID -> interpret Base.Config {..} fissionURL cmd
      Left  err       -> return . Left $ include err

interpret ::
  MonadIO m
  => Base.Config
  -> BaseUrl
  -> Command
  -> m (Either (OpenUnion Errs) ())
interpret baseCfg@Base.Config {ipfsDaemonVar} fissionURL cmd =
  runFissionCLI baseCfg $
    dispatch `always` do
      liftIO (tryReadMVar ipfsDaemonVar) >>= \case
        Nothing     -> return ()
        Just daemon -> IPFS.Daemon.stop daemon

  where
    dispatch :: FissionCLI Errs Base.Config ()
    dispatch = do
      logDebug . Text.pack $ show cmd

      case cmd of
        Version _ ->
          logInfo $ maybe "unknown" identity (Meta.version =<< Meta.package)

        Setup Setup.Options {forceOS} ->
          Setup.setup forceOS fissionURL

        App subCmd ->
          App.interpret baseCfg subCmd

        User subCmd ->
          case subCmd of
            Register _ -> void Handler.register
            WhoAmI   _ -> Handler.whoami

  -- FIXME remove when you have better erroro rtyes
instance Display String where
  textDisplay = Text.pack

instance Display CryptoError where
  textDisplay = Text.pack . show

instance Display RSA.Error where
  textDisplay = Text.pack . show

instance Exception String
instance Exception RSA.Error

finalizeDID ::
  MonadIO m
  => Maybe DID
  -> Base.Config
  -> m (Either (OpenUnion Errs) DID)
finalizeDID (Just did) _ =
  pure $ Right did

finalizeDID Nothing baseCfg@Base.Config {fissionURL} =
  runFissionCLI baseCfg do
    attempt Env.get >>= \case
      Right Env {serverDID} -> return serverDID
      Left  _               -> Env.fetchServerDID fissionURL
