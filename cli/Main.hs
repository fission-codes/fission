module Main (main) where

import           Fission.Prelude
import qualified RIO.Partial as Partial

import           Network.HTTP.Client     as HTTP
import           Network.HTTP.Client.TLS as HTTP
import           Servant.Client

import           Fission.Environment
import           Fission.Internal.App (isDebugEnabled, setRioVerbose)
import qualified Fission.Web.Client as Client

import qualified Network.IPFS.BinPath.Types as IPFS
import qualified Network.IPFS.Timeout.Types as IPFS

import           Fission.CLI
import qualified Fission.CLI.Config.Base.Types as CLI

main :: IO ()
main = do
  isVerbose <- isDebugEnabled
  setRioVerbose isVerbose

  logOptions <- logOptionsHandle stderr isVerbose
  processCtx <- mkDefaultProcessContext

  ipfsPath    <- withEnv "IPFS_PATH" (IPFS.BinPath "/usr/local/bin/ipfs") IPFS.BinPath
  ipfsTimeout <- withEnv "IPFS_TIMEOUT" (IPFS.Timeout 3600) (IPFS.Timeout . Partial.read)

  isTLS <- getFlag "FISSION_TLS" .!~ True
  path  <- withEnv "FISSION_ROOT" "" identity
  host  <- withEnv "FISSION_HOST" "runfission.com" identity
  port  <- withEnv "FISSION_PORT" (if isTLS then 443 else 80) Partial.read
  tOut  <- withEnv "FISSION_TIMEOUT" 1800000000 Partial.read

  let
    rawHTTPSettings =
      if isTLS
        then tlsManagerSettings
        else defaultManagerSettings

  httpManager <- HTTP.newManager <| rawHTTPSettings
    { managerResponseTimeout = responseTimeoutMicro tOut }

  let
    url        = BaseUrl (if isTLS then Https else Http) host port path
    fissionAPI = Client.Runner (Client.request httpManager url)

  withLogFunc logOptions \logFunc -> cli CLI.BaseConfig {..}
