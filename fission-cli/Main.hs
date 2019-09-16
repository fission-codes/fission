module Main (main) where

import           RIO
import qualified RIO.Partial as Partial

import qualified Network.HTTP.Client as HTTP
import           Servant.Client

import System.Environment (lookupEnv)
import System.FSNotify

import           Fission.CLI
import qualified Fission.CLI.Types   as CLI
import           Fission.Environment

import qualified Fission.Web.Client as Client

main :: IO ()
main = do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose

  isTLS <- getFlag "FISSION_TLS" .!~ True
  path  <- withEnv "FISSION_ROOT" "" id
  host  <- withEnv "FISSION_HOST" "runfission.com" id
  port  <- withEnv "FISSION_PORT" (if isTLS then 443 else 80) Partial.read

  let scheme = if isTLS then Https else Http
  let url    = BaseUrl scheme host port path

  withManager \mgr -> withLogFunc logOptions \logger -> do
    let cfg = CLI.Config
                { _fissionAPI = Client.Runner $ Client.request httpManager url
                , _logFunc    = logger
                , _watchMgr   = mgr
                }
    runRIO cfg . logDebug $ "Requests will be made to " <> displayShow url
    (_, runCLI) <- cli cfg
    runCLI
