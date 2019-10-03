module Fission.Internal.Debug (runFissionApp) where

import           RIO
import           RIO.Process (mkDefaultProcessContext)

import           Database.Selda.PostgreSQL
import qualified Network.HTTP.Client as HTTP
import           Servant.Client

import           Fission.Config.Types
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Storage.PostgreSQL (connPool)
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Platform.Heroku.Types          as Hku

runFissionApp :: RIO Config a -> IO a
runFissionApp action = do
  logOptions   <- logOptionsHandle stdout True
  withLogFunc (setLogUseTime True logOptions) $ \_logFunc -> do
    _dbPool      <- runSimpleApp $ connPool 1 1 3600 _pgConnectInfo
    _processCtx  <- mkDefaultProcessContext
    _httpManager <- HTTP.newManager HTTP.defaultManagerSettings
    runRIO Config {..} do
      logDebug . displayShow =<< ask
      action
  where
    _herokuID       = Hku.ID       "HEROKU_ID"
    _herokuPassword = Hku.Password "HEROKU_PASSWORD"

    _ipfsPath    = "/usr/local/bin/ipfs"
    _ipfsURL     = IPFS.URL $ BaseUrl Http "localhost" 5001 ""
    _ipfsTimeout = IPFS.Timeout 3600

    _pgConnectInfo = PGConnectInfo "localhost" 5432 "web_api" Nothing Nothing Nothing

    _host = "mycoolapp.io"
