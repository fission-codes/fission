module Fission.Internal.Development.Config
  ( runFissionApp
  , runNewFissionApp
  , mkConfig
  ) where

import           RIO
import           RIO.Process (ProcessContext, mkDefaultProcessContext)

import           Database.Selda.PostgreSQL
import qualified Network.HTTP.Client as HTTP
import           Servant.Client

import           Fission.Config.Types (Config (..))
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Storage.PostgreSQL (connPool)
import qualified Fission.Storage.Types         as DB
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Platform.Heroku.Types as Hku

runNewFissionApp :: RIO Config b -> IO b
runNewFissionApp action = do
  logOptions   <- logOptionsHandle stdout True
  withLogFunc (setLogUseTime True logOptions) $ \_logFunc -> do
    let _pgConnectInfo = PGConnectInfo "localhost" 5432 "web_api" Nothing Nothing Nothing
    _dbPool      <- runSimpleApp $ connPool 1 1 3600 _pgConnectInfo
    _processCtx  <- mkDefaultProcessContext
    _httpManager <- HTTP.newManager HTTP.defaultManagerSettings
    runFissionApp _logFunc _dbPool _processCtx _httpManager action

runFissionApp :: MonadIO m
              => LogFunc
              -> DB.Pool
              -> ProcessContext
              -> HTTP.Manager
              -> RIO Config a
              -> m a
runFissionApp _logFunc _dbPool _processCtx _httpManager action =
  runRIO config do
    logDebug $ displayShow config
    action
  where
    config = Config {..}

    _herokuID       = Hku.ID       "HEROKU_ID"
    _herokuPassword = Hku.Password "HEROKU_PASSWORD"

    _ipfsPath    = "/usr/local/bin/ipfs"
    _ipfsURL     = IPFS.URL $ BaseUrl Http "localhost" 5001 ""
    _ipfsTimeout = IPFS.Timeout 3600

    _pgConnectInfo = pgConnectInfo

    _host = "mycoolapp.io"

mkConfig :: IO Config
mkConfig = do
  let
    _herokuID       = Hku.ID       "HEROKU_ID"
    _herokuPassword = Hku.Password "HEROKU_PASSWORD"
    _ipfsPath       = "/usr/local/bin/ipfs"
    _ipfsURL        = IPFS.URL $ BaseUrl Http "localhost" 5001 ""
    _ipfsTimeout    = IPFS.Timeout 3600
    _pgConnectInfo  = pgConnectInfo
    _host           = "mycoolapp.io"

  _dbPool       <- runSimpleApp $ connPool 1 1 3600 _pgConnectInfo
  _processCtx   <- mkDefaultProcessContext
  _httpManager  <- HTTP.newManager HTTP.defaultManagerSettings

  -- A bit dirty; doesn't handle teardown
  logOptions           <- logOptionsHandle stdout True
  (_logFunc, _ :: IO ()) <- newLogFunc $ setLogUseTime True logOptions
  return Config {..}

pgConnectInfo :: PGConnectInfo
pgConnectInfo = PGConnectInfo "localhost" 5432 "web_api" Nothing Nothing Nothing
