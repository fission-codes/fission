-- | Development helpers, primarily for REPL
module Fission.Internal.Development
  ( run
  , runOne
  , mkConfig
  , mkConfig'
  , pgConnectInfo'
  ) where

import           Database.Selda.PostgreSQL
import qualified Network.HTTP.Client as HTTP
import           Servant.Client

import           Fission.Prelude
import           Fission.Config.Types (Config (..))
import           Fission.Internal.Orphanage.RIO ()
import           Fission.Storage.PostgreSQL (connPool)
import qualified Fission.Storage.Types         as DB
import qualified Fission.IPFS.Types            as IPFS
import qualified Fission.Platform.Heroku.Types as Hku
import           Fission.Web.Types

{- | Setup a config, run an action in it, and tear down the config.
     Great for quick one-offs, but anything with heavy setup
     should use 'mkConfig' or 'run'.

     == Example Use

     > runOne Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
runOne :: RIO Config a -> IO a
runOne action = do
  logOptions   <- logOptionsHandle stdout True
  withLogFunc (setLogUseTime True logOptions) \logFunc -> do
    dbPool      <- runSimpleApp <| connPool 1 1 3600 pgConnectInfo'
    processCtx  <- mkDefaultProcessContext
    httpManager <- HTTP.newManager HTTP.defaultManagerSettings
    run logFunc dbPool processCtx httpManager action

{- | Run some action(s) in the app's context,
     but asks for existing portions of the setup that require side effects,
     in case they're already available (which is more efficient).

     == Example Use

     > dbPool <- runSimpleApp $ connPool 1 1 3600 pgConnectInfo'
     > processCtx <- mkDefaultProcessContext
     > httpManager <- HTTP.newManager HTTP.defaultManagerSettings
     > logOptions <- logOptionsHandle stdout True
     > (logFunc,  :: IO ()) <- newLogFunc $ setLogUseTime True logOptions
     >
     > let runSession = run logFunc dbPool processCtx httpManager
     >
     > runSession Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > runSession Fission.IPFS.Peer.connect Fission.peer
     > -- ()
-}
run :: LogFunc
    -> DB.Pool
    -> ProcessContext
    -> HTTP.Manager
    -> RIO Config a
    -> IO a
run logFunc dbPool processCtx httpManager action =
  runRIO config do
    logDebug <| displayShow config
    action
  where
    config = Config {..}

    host          = Host <| BaseUrl Https "mycoolapp.io" 443 ""
    pgConnectInfo = pgConnectInfo'

    herokuID       = Hku.ID       "HEROKU_ID"
    herokuPassword = Hku.Password "HEROKU_PASSWORD"

    ipfsPath    = "/usr/local/bin/ipfs"
    ipfsURL     = IPFS.URL <| BaseUrl Http "localhost" 5001 ""
    ipfsTimeout = IPFS.Timeout 3600

    awsAccessKey  = "SOME_AWS_ACCESS_KEY"
    awsSecretKey  = "SOME_AWS_SECRET_KEY"
    awsZoneID     = "SOME_AWS_ZONE_ID"
    awsDomainName = "SOME_AWS_DOMAIN_NAME"

{- | Setup a complete development configuration with all pure defaults set

     == Example Use

     > dbPool       <- runSimpleApp $ connPool 1 1 3600 pgConnectInfo'
     > processCtx   <- mkDefaultProcessContext
     > httpManager  <- HTTP.newManager HTTP.defaultManagerSettings
     > logOptions   <- logOptionsHandle stdout True
     > (logFunc, ) <- newLogFunc $ setLogUseTime True logOptions
     >
     > let cfg = mkConfig dbPool processCtx httpManager logFunc
     > let run' = runRIO cfg
     >
     > run' Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > run' Fission.IPFS.Peer.connect Fission.peer
     > -- ()

     If you need to overwrite any fields: use record update syntax, or the 'Config' lenses.

     > let run' = runRIO cfg { ipfsPath = "~/Downloads/ipfs" }
     > run' Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
mkConfig :: DB.Pool -> ProcessContext -> HTTP.Manager -> LogFunc -> Config
mkConfig dbPool processCtx httpManager logFunc = Config {..}
  where
    host          = Host <| BaseUrl Https "mycoolapp.io" 443 ""
    pgConnectInfo = pgConnectInfo'

    herokuID       = Hku.ID       "HEROKU_ID"
    herokuPassword = Hku.Password "HEROKU_PASSWORD"

    ipfsPath       = "/usr/local/bin/ipfs"
    ipfsURL        = IPFS.URL <| BaseUrl Http "localhost" 5001 ""
    ipfsTimeout    = IPFS.Timeout 3600

    awsAccessKey  = "SOME_AWS_ACCESS_KEY"
    awsSecretKey  = "SOME_AWS_SECRET_KEY"
    awsZoneID     = "SOME_AWS_ZONE_ID"
    awsDomainName = "SOME_AWS_DOMAIN_NAME"

{- | Setup a complete development configuration.

     Note that this does not clean up the log function,
     but does return an action to do so.

     == Example Use

     > (cfg, ) <- mkConfig'
     > let run' = runRIO cfg
     > run' Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
     >
     > run' Fission.IPFS.Peer.connect Fission.peer
     > -- ()

     If you need to overwrite any fields: use record update syntax, or the 'Config' lenses.

     > (cfg, ) <- mkConfig'
     > let run' = runRIO cfg { ipfsPath = "~/Downloads/ipfs" }
     > run' Fission.IPFS.Peer.all
     > -- Right ["/ip4/3.215.160.238/tcp/4001/ipfs/QmVLEz2SxoNiFnuyLpbXsH6SvjPTrHNMU88vCQZyhgBzgw"]
-}
mkConfig' :: IO (Config, IO ())
mkConfig' = do
  dbPool      <- runSimpleApp <| connPool 1 1 3600 pgConnectInfo'
  processCtx  <- mkDefaultProcessContext
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  -- A bit dirty; doesn't handle teardown
  logOptions       <- logOptionsHandle stdout True
  (logFunc, close) <- newLogFunc <| setLogUseTime True logOptions

  let cfg = mkConfig dbPool processCtx httpManager logFunc
  return (cfg, close)

pgConnectInfo' :: PGConnectInfo
pgConnectInfo' = PGConnectInfo "localhost" 5432 "webapi" Nothing Nothing Nothing
