module Main (main) where

import           RIO
import qualified RIO.Partial as Partial

import           Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS as HTTP
import           Servant.Client

import System.Environment (lookupEnv)
import qualified System.FSNotify as FS

import           Fission.CLI
import qualified Fission.CLI.Types   as CLI
import           Fission.Environment

import qualified Fission.Web.Client as Client

main :: IO ()
main = do
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose

  isTLS   <- getFlag "FISSION_TLS" .!~ True
  path    <- withEnv "FISSION_ROOT" "" id
  host    <- withEnv "FISSION_HOST" "runfission.com" id
  port    <- withEnv "FISSION_PORT" (if isTLS then 443 else 80) Partial.read
  timeout <- withEnv "FISSION_TIMEOUT" 1800000000 Partial.read

  let url = BaseUrl (if isTLS then Https else Http) host port path
  let rawHTTPSettings = if isTLS
                           then tlsManagerSettings
                           else defaultManagerSettings

  httpManager <- HTTP.newManager $ rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro timeout }

  withLogFunc logOptions \logger -> do
  -- FS.withManager \watcher -> withLogFunc logOptions \logger -> do
    let cfg = CLI.Config
                { _fissionAPI = Client.Runner $ Client.request httpManager url
                , _logFunc    = logger
                -- , _watchMgr   = watcher
                }

    runRIO cfg . logDebug $ "Requests will be made to " <> displayShow url
    (_, runCLI) <- cli cfg
    runCLI
