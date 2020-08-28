module Fission.CLI (cli, interpret) where

import qualified Crypto.PubKey.Ed25519                 as Ed25519
import           Options.Applicative
import qualified RIO.Text                              as Text

import           Network.HTTP.Client                   as HTTP
import           Network.HTTP.Client.TLS               as HTTP
import           Servant.Client.Core

import           Fission.Prelude

import           Fission.Error
import qualified Fission.Internal.CLI.Meta             as Meta

import qualified Fission.CLI.Base.Types                as Base
import qualified Fission.CLI.Handler                   as Handler

import           Fission.CLI.Parser                    as CLI
import           Fission.CLI.Parser.Command.Types
import           Fission.CLI.Parser.Command.User.Types
import           Fission.CLI.Parser.Types              as Parser
import           Fission.CLI.Parser.Verbose.Types

import qualified Fission.CLI.App                       as App
import           Fission.CLI.Types

type Errs = AlreadyExists Ed25519.SecretKey ': App.Errs

cli :: IO (Either (OpenUnion Errs) ())
cli = do
  Parser.Options
    { fissionDID = cachedServerDID
    , fissionURL
    , cmd
    } <- execParser CLI.parserWithInfo

  let
    VerboseFlag isVerbose = getter cmd

    rawHTTPSettings =
      case baseUrlScheme fissionURL of
        Http  -> defaultManagerSettings
        Https -> tlsManagerSettings

  httpManager <- HTTP.newManager rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro 1_800_000_000 }

  processCtx <- mkDefaultProcessContext
  logOptions <- logOptionsHandle stderr isVerbose

  withLogFunc logOptions \logFunc ->
    interpret Base.Config {..} cmd

interpret ::
  MonadIO m
  => Base.Config
  -> Command
  -> m (Either (OpenUnion Errs) ())
interpret baseCfg cmd =
  runFissionCLI baseCfg do
    logDebug . Text.pack $ show cmd

    case cmd of
      Version _ ->
        logInfo $ maybe "unknown" identity (Meta.version =<< Meta.package)

      App subCmd ->
        App.interpret baseCfg subCmd

      User subCmd ->
        case subCmd of
          Register _ -> Handler.setup
          WhoAmI   _ -> Handler.whoami
