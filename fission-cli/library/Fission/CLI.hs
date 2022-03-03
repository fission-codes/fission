module Fission.CLI (cli, interpret) where

import qualified Data.Version                                       as Version
import qualified Paths_fission_cli                                  as CLI

import qualified RIO                                                as Logger

import           Options.Applicative

import           Network.HTTP.Client                                as HTTP
import           Network.HTTP.Client.TLS                            as HTTP

import           Servant.Client.Core

import qualified Network.IPFS.Timeout.Types                         as IPFS
import qualified Network.IPFS.URL.Types                             as IPFS

import           Fission.Prelude

import           Web.DID.Types

import qualified Fission.CLI.Internal.ServerDID                     as ServerDID

import qualified Fission.CLI.Base.Types                             as Base
import           Fission.CLI.Environment                            as Env
import qualified Fission.CLI.IPFS.Daemon                            as IPFS.Daemon

import           Fission.CLI.Release
import qualified Fission.CLI.Remote                                 as Remote

import           Fission.CLI.Parser                                 as CLI
import           Fission.CLI.Parser.Command.Setup.Types             as Setup
import           Fission.CLI.Parser.Command.Types
import           Fission.CLI.Parser.Types                           as Parser
import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.App                                    as App
import           Fission.CLI.Types

import           Fission.CLI.Handler.Error.Types                    (Errs)
import qualified Fission.CLI.Handler.Setup                          as Setup
import qualified Fission.CLI.Handler.User                           as User

import           Fission.Internal.Orphanage.Crypto.Error            ()
import           Fission.Internal.Orphanage.Crypto.PubKey.RSA.Error ()
import           Fission.Internal.Orphanage.Yaml.ParseException     ()

cli :: MonadUnliftIO m => m (Either (OpenUnion Errs) ())
cli = do
  Parser.Options
    { fissionDID
    , remote
    , cmd
    , verboseFlag = VerboseFlag isVerbose
    } <- liftIO $ customExecParser (prefs showHelpOnError) CLI.parserWithInfo

  let
    ipfsURL     = IPFS.URL $ BaseUrl Https "ipfs.io" 443 ""
    ipfsTimeout = IPFS.Timeout 3600

    rawHTTPSettings =
      case baseUrlScheme $ Remote.toBaseUrl remote of
        Http  -> defaultManagerSettings
        Https -> tlsManagerSettings

  tlsManager <- liftIO $ HTTP.newManager tlsManagerSettings
  httpManager <- liftIO $ HTTP.newManager rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro 1_800_000_000 }

  ipfsDaemonVar <- liftIO newEmptyMVar
  processCtx    <- mkDefaultProcessContext
  logOptions'   <- logOptionsHandle stderr isVerbose

  let
    logOptions =
      if isVerbose
        then logOptions'
        else setLogMinLevel Logger.LevelError logOptions'

  withLogFunc (setLogUseLoc False logOptions) \logFunc -> do
    finalizeDID fissionDID Base.Config {serverDID = ServerDID.fallback, ..} >>= \case
      Right serverDID -> interpret Base.Config {..} cmd
      Left  err       -> return . Left $ include err

interpret :: MonadIO m => Base.Config -> Command -> m (Either (OpenUnion Errs) ())
interpret baseCfg@Base.Config {ipfsDaemonVar} cmd =
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
        Version _ -> do
          logUser $ Version.showVersion CLI.version

        Setup Setup.Options {forceOS, maybeUsername, maybeEmail, maybeKeyFile} ->
          Setup.setup forceOS maybeUsername maybeEmail maybeKeyFile

        App subCmd ->
          App.interpret baseCfg subCmd

        User subCmd ->
          User.interpret subCmd

finalizeDID :: MonadIO m => Maybe DID -> Base.Config -> m (Either (OpenUnion Errs) DID)
finalizeDID (Just did) _ =
  pure $ Right did

finalizeDID Nothing baseCfg@Base.Config {remote} =
  runFissionCLI @Errs baseCfg do
    attempt Env.get >>= \case
      Right Env {serverDID} -> return serverDID
      Left  _               -> Env.fetchServerDID $ Remote.toBaseUrl remote
