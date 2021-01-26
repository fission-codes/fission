module Fission.CLI (cli, interpret) where

import           Data.Type.List

import qualified Crypto.PubKey.Ed25519                              as Ed25519
import           Options.Applicative

import           Network.HTTP.Client                                as HTTP
import           Network.HTTP.Client.TLS                            as HTTP
import qualified Network.IPFS.Process.Error                         as IPFS.Process

import           Servant.Client.Core

import qualified Network.IPFS.Timeout.Types                         as IPFS
import qualified Network.IPFS.URL.Types                             as IPFS

import           Fission.Prelude

import           Fission.Error

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error                        as Username

import qualified Fission.CLI.IPFS.Daemon                            as IPFS.Daemon
import qualified Fission.CLI.Meta                                   as Meta

import           Fission.CLI.Environment                            as Env
import qualified Fission.CLI.Environment.OS                         as OS

import qualified Fission.CLI.Base.Types                             as Base

-- import qualified Fission.CLI.Handler                                as Handler
import qualified Fission.CLI.Handler.Setup                          as Setup

import           Fission.CLI.Release
import qualified Fission.CLI.Remote                                 as Remote

import           Fission.CLI.Parser                                 as CLI
import           Fission.CLI.Parser.Command.Setup.Types             as Setup
import           Fission.CLI.Parser.Command.Types
-- import           Fission.CLI.Parser.Command.User.Register.Types     as Register
-- import           Fission.CLI.Parser.Command.User.Types
import           Fission.CLI.Parser.Types                           as Parser
import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.App                                    as App
import           Fission.CLI.Types

import qualified Fission.CLI.Handler.User                           as User

import qualified Fission.CLI.Internal.ServerDID                     as ServerDID

import           Fission.Internal.Orphanage.Crypto.Error            ()
import           Fission.Internal.Orphanage.Crypto.PubKey.RSA.Error ()
import           Fission.Internal.Orphanage.Yaml.ParseException     ()

type Errs = BaseErrs ++ App.Errs ++ User.Errs

type BaseErrs
   = '[ OS.Unsupported
      , AlreadyExists Ed25519.SecretKey
      , ClientError
      , Username.Invalid
      , IPFS.Process.Error
      ]

cli :: MonadUnliftIO m => m (Either (OpenUnion Errs) ())
cli = do
  Parser.Options {fissionDID, remote, cmd} <- liftIO $ customExecParser (prefs showHelpOnError) CLI.parserWithInfo

  let
    VerboseFlag isVerbose = getter cmd

    ipfsURL     = IPFS.URL $ BaseUrl Https "ipfs.io" 443 ""
    ipfsTimeout = IPFS.Timeout 3600

    rawHTTPSettings =
      case baseUrlScheme $ Remote.toBaseUrl remote of
        Http  -> defaultManagerSettings
        Https -> tlsManagerSettings

  httpManager <- liftIO $ HTTP.newManager rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro 1_800_000_000 }

  ipfsDaemonVar <- liftIO newEmptyMVar
  processCtx    <- mkDefaultProcessContext
  logOptions    <- logOptionsHandle stderr isVerbose

  withLogFunc logOptions \logFunc -> do
    finalizeDID fissionDID Base.Config {serverDID = ServerDID.fallback, ..} >>= \case
      Right serverDID -> interpret Base.Config {..} (Remote.toBaseUrl remote) cmd
      Left  err       -> return . Left $ include err

interpret :: MonadIO m => Base.Config -> BaseUrl -> Command -> m (Either (OpenUnion Errs) ())
interpret baseCfg@Base.Config {ipfsDaemonVar} fissionURL cmd =
  runFissionCLI baseCfg $
    dispatch `always` do
      liftIO (tryReadMVar ipfsDaemonVar) >>= \case
        Nothing     -> return ()
        Just daemon -> IPFS.Daemon.stop daemon

  where
    dispatch :: FissionCLI Errs Base.Config ()
    dispatch = do
      logDebug $ display cmd

      checkLatestRelease

      case cmd of
        Version _ ->
          logInfo $ maybe "unknown" identity (Meta.version =<< Meta.package)

        Setup Setup.Options {forceOS, maybeUsername, maybeEmail} ->
          Setup.setup forceOS fissionURL maybeUsername maybeEmail

        App subCmd ->
          App.interpret baseCfg subCmd

        User subCmd ->
          User.interpret baseCfg subCmd

finalizeDID :: MonadIO m => Maybe DID -> Base.Config -> m (Either (OpenUnion Errs) DID)
finalizeDID (Just did) _ =
  pure $ Right did

finalizeDID Nothing baseCfg@Base.Config {remote} =
  runFissionCLI baseCfg do
    attempt Env.get >>= \case
      Right Env {serverDID} -> return serverDID
      Left  _               -> Env.fetchServerDID $ Remote.toBaseUrl remote
