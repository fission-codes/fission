module Main (main) where

import           RIO
import qualified RIO.Partial as Partial

import qualified Network.HTTP.Client as HTTP
import           Servant.Client
import           System.Environment  (lookupEnv)

import Fission.CLI             as CLI
import Fission.Environment
import Fission.Web.Auth.Client as Fission.Auth

main :: IO ()
main = do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  verbose     <- isJust <$> lookupEnv "RIO_VERBOSE"
  logOptions  <- logOptionsHandle stderr verbose

  isTLS <- getFlag "FISSION_TLS"
  path  <- withEnv "FISSION_ROOT" "" id
  host  <- withEnv "FISSION_HOST" "localhost" id -- TODO default to prod
  port  <- withEnv "FISSION_PORT" (if isTLS then 443 else 80) Partial.read

  let scheme = if isTLS then Https else Http
  let url    = BaseUrl scheme host port path

  withLogFunc logOptions $ \logger -> do
    let cfg = CLI.Config
                { _fissionAPI = ClientRunner $ Fission.Auth.run httpManager url
                , _logFunc    = logger
                }
    runRIO cfg . logDebug $ "Requests will be made to " <> displayShow url
    (_, runCLI) <- cli cfg
    runCLI
