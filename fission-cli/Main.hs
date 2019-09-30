module Main (main) where

import           RIO
import qualified RIO.Partial as Partial
import           RIO.Process (mkDefaultProcessContext)

import           Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS as HTTP
import           Servant.Client
import           System.Environment (lookupEnv)

import           Fission.Environment
import qualified Fission.Web.Client as Client

import           Fission.CLI
import qualified Fission.CLI.Types   as CLI

main :: IO ()
main = do
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose
  _processCtx <- mkDefaultProcessContext

  _ipfsPath <- withEnv "IPFS_PATH" "" id .!~ False

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

  let _fissionAPI = Client.Runner $ Client.request httpManager url

  withLogFunc logOptions \_logFunc -> do
    runRIO _logFunc . logDebug $ "Requests will be made to " <> displayShow url
    (_, runCLI) <- cli CLI.Config {..}
    runCLI
