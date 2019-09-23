module Main (main) where

import           RIO
import qualified RIO.Partial as Partial

import           SuperRecord as SR

import qualified Network.HTTP.Client as HTTP
import           Servant.Client
import           System.Environment  (lookupEnv)

import           Fission.CLI
import           Fission.Environment
import           Fission.Internal.Orphanage.Rec ()

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

  let

  withLogFunc logOptions $ \logger -> do
    let
      url = BaseUrl (if isTLS then Https else Http) host port path
      cfg = #fissionAPI := Client.Runner (Client.request httpManager url)
       SR.& #logFunc    := logger
       SR.& rnil

    runRIO cfg . logDebug $ "Requests will be made to " <> displayShow url
    (_, runCLI) <- cli cfg
    runRIO cfg runCLI
