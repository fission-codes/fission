module Fission.Internal.Reload (update) where

import RIO
import RIO.ByteString
import RIO.Process (mkDefaultProcessContext)

import qualified Network.HTTP.Client                  as HTTP
import           Network.Wai.Handler.Warp

import           Fission.Internal.Orphanage.RIO ()
import           Fission.Internal.UTF8
import           Fission.Storage.PostgreSQL     (connPool)

import qualified Fission.Web       as Web
import qualified Fission.Web.Log   as Web.Log

import System.Remote.Monitoring.Wai
import Fission.Internal.Development
import Rapid


import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import System.Directory (doesFileExist)
import System.Environment


update :: IO ()
update = rapid 0 \r -> do
  -- dbVar <- createRef r "Database Var" do -- newEmptyTMVarIO
  -- -- start r "Database" do
  --   runSimpleApp $ connPool 1 1 3600 pgConnectInfo
  --   -- pool <- runSimpleApp $ connPool 1 1 3600 pgConnectInfo
  --   -- atomically $ putTMVar dbVar pool

  -- processVar <- createRef r "Process Context Var" do -- newEmptyTMVarIO
  -- -- start r "Process Context" do
  --   mkDefaultProcessContext
  --   -- processCtx  <- mkDefaultProcessContext
  --   -- atomically $ putTMVar processVar processCtx

  -- httpVar <- createRef r "HTTP Manager Var" do--  newEmptyTMVarIO
  -- -- start r "Start HTTP Manager" do
  --   HTTP.newManager HTTP.defaultManagerSettings
  --   -- httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  --   -- atomically $ putTMVar httpVar httpManager

  logVar <- createRef r "Logger Var" do -- $ newEmptyTMVarIO
  -- start r "Logger" do
    logOptions <- logOptionsHandle stdout True
    (logFunc, _ :: IO ()) <- newLogFunc $ setLogUseTime True logOptions
    return logFunc
    -- atomically $ putTMVar loggerVar logFunc

  cfg <- createRef r "Config" do
    dbVar <- runSimpleApp $ connPool 1 1 3600 pgConnectInfo
    processVar <- mkDefaultProcessContext
    httpVar <- HTTP.newManager HTTP.defaultManagerSettings
    -- logOptions <- logOptionsHandle stdout True
    -- (logFunc, _ :: IO ()) <- newLogFunc $ setLogUseTime True logOptions

    return $ mkConfig dbVar processVar httpVar logVar

  -- start r "EKG" do
  start r "EKG" do
    void $ forkServer "localhost" 9630
    putText "\n📈 EKG live at http://localhost:9630\n"

  restart r "Web Server" do
    -- dbPool      <- atomically $ takeTMVar dbVar
    -- processCtx  <- atomically $ takeTMVar processVar
    -- httpManager <- atomically $ takeTMVar httpVar
    -- logFunc     <- atomically $ takeTMVar loggerVar

    -- let cfg = mkConfig dbPool processCtx httpManager logFunc

    putText "\n🔄 Web server up at port 8000\n"
    runRIO cfg do
      liftIO . runSettings (Web.Log.mkSettings logVar 8000) =<< Web.app
