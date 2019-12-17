-- | Development helpers, primarily for REPL
module Fission.Internal.Development
  ( run
  , runOne
  , mkConfig
  , mkConfig'
  , connectionInfo
  ) where

import           Data.Pool
import           Database.Persist.Sql (SqlBackend)

import qualified Network.IPFS.Types  as IPFS
import qualified Network.HTTP.Client as HTTP
import           Servant.Client

import           Fission
import           Fission.Prelude

import qualified Fission.AWS.Types as AWS
import           Fission.Web.Types

import qualified Fission.Platform.Heroku.ID.Types       as Hku
import qualified Fission.Platform.Heroku.Password.Types as Hku
import           Fission.Storage.PostgreSQL.ConnectionInfo.Types

import           Fission.Storage.PostgreSQL

{- | Setup a config, run an action in it, and tear down the config.
     Great for quick one-offs, but anything with heavy setup
     should use 'mkConfig' or 'run'.

     == Example Use

     > runOne Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
runOne :: Fission a -> IO a
runOne action = do
  logOptions <- logOptionsHandle stdout True
  processCtx  <- mkDefaultProcessContext
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  withLogFunc (setLogUseTime True logOptions) \logFunc -> do
    withDBPool logFunc connectionInfo (PoolSize 4) \dbPool ->
      action
        |> run logFunc dbPool processCtx httpManager
        |> liftIO

{- | Run some action(s) in the app's context,
     but asks for existing portions of the setup that require side effects,
     in case they're already available (which is more efficient).

     == Example Use

     > dbPool <- runApp $ connPool 1 1 3600 pgConnectInfo'
     > processCtx <- mkDefaultProcessContext
     > httpManager <- HTTP.newManager HTTP.defaultManagerSettings
     > logOptions <- logOptionsHandle stdout True
     > (logFunc,  :: IO ()) <- newLogFunc $ setLogUseTime True logOptions
     >
     > let runSession = run logFunc dbPool processCtx httpManager
     >
     > runSession Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > runSession Network.IPFS.Peer.connect Fission.peer
     > -- ()
-}
run ::
     LogFunc
  -> Pool SqlBackend
  -> ProcessContext
  -> HTTP.Manager
  -> Fission a
  -> IO a
run logFunc dbPool processCtx httpManager action =
  runFission config do
    logDebug <| textShow config
    action
  where
    config = Config {..}

    host = Host <| BaseUrl Https "mycoolapp.io" 443 ""

    herokuID       = Hku.ID       "HEROKU_ID"
    herokuPassword = Hku.Password "HEROKU_PASSWORD"

    ipfsPath       = "/usr/local/bin/ipfs"
    ipfsURL        = IPFS.URL <| BaseUrl Http "localhost" 5001 ""
    ipfsTimeout    = IPFS.Timeout 3600
    ipfsGateway    = IPFS.Gateway "ipfs.runfission.com"
    ipfsRemotePeer = IPFS.Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"

    awsAccessKey          = "SOME_AWS_ACCESS_KEY"
    awsSecretKey          = "SOME_AWS_SECRET_KEY"
    awsZoneID             = "SOME_AWS_ZONE_ID"
    awsDomainName         = "SOME_AWS_DOMAIN_NAME"
    awsRoute53MockEnabled = AWS.Route53MockEnabled True

{- | Setup a complete development configuration with all pure defaults set

     == Example Use

     > dbPool       <- runApp $ connPool 1 1 3600 pgConnectInfo'
     > processCtx   <- mkDefaultProcessContext
     > httpManager  <- HTTP.newManager HTTP.defaultManagerSettings
     > logOptions   <- logOptionsHandle stdout True
     > (logFunc, ) <- newLogFunc $ setLogUseTime True logOptions
     >
     > let cfg = mkConfig dbPool processCtx httpManager logFunc
     > let run' = runFission cfg
     >
     > run' Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > run' Network.IPFS.Peer.connect Fission.peer
     > -- ()

     If you need to overwrite any fields: use record update syntax, or the 'Config' lenses.

     > let run' = runFission cfg { ipfsPath = "~/Downloads/ipfs" }
     > run' Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
mkConfig ::
     Pool SqlBackend
  -> ProcessContext
  -> HTTP.Manager
  -> LogFunc
  -> Config
mkConfig dbPool processCtx httpManager logFunc = Config {..}
  where
    host = Host <| BaseUrl Https "mycoolapp.io" 443 ""

    herokuID       = Hku.ID       "HEROKU_ID"
    herokuPassword = Hku.Password "HEROKU_PASSWORD"

    ipfsPath       = "/usr/local/bin/ipfs"
    ipfsURL        = IPFS.URL <| BaseUrl Http "localhost" 5001 ""
    ipfsRemotePeer = IPFS.Peer "/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"
    ipfsTimeout    = IPFS.Timeout 3600
    ipfsGateway    = IPFS.Gateway "ipfs.runfission.com"

    awsAccessKey          = "SOME_AWS_ACCESS_KEY"
    awsSecretKey          = "SOME_AWS_SECRET_KEY"
    awsZoneID             = "SOME_AWS_ZONE_ID"
    awsDomainName         = "SOME_AWS_DOMAIN_NAME"
    awsRoute53MockEnabled = AWS.Route53MockEnabled True

{- | Setup a complete development configuration.

     Note that this does not clean up the log function,
     but does return an action to do so.

     == Example Use

     > (cfg, ) <- mkConfig'
     > let run' = runFission cfg
     > run' Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > run' Network.IPFS.Peer.connect Fission.peer
     > -- ()

     If you need to overwrite any fields: use record update syntax, or the 'Config' lenses.

     > (cfg, ) <- mkConfig'
     > let run' = runFission cfg { ipfsPath = "~/Downloads/ipfs" }
     > run' Network.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
mkConfig' :: IO (Config, IO ())
mkConfig' = do
  processCtx  <- mkDefaultProcessContext
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  -- A bit dirty; doesn't directly handle teardown
  (logFunc, close) <- newLogFunc . setLogUseTime True =<< logOptionsHandle stdout True

  withDBPool logFunc connectionInfo (PoolSize 4) \dbPool -> do
    let cfg = mkConfig dbPool processCtx httpManager logFunc
    return (cfg, close)

connectionInfo :: ConnectionInfo
connectionInfo = ConnectionInfo
  { pgDatabase = "web_api"
  , pgHost     = "localhost"
  , pgPort     = 5432
  , pgUsername = Nothing
  , pgPassword = Nothing
  }
