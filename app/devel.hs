module Main where

import Prelude

import RIO
import RIO.ByteString
import RIO.Process    (mkDefaultProcessContext)

import qualified Network.HTTP.Client      as HTTP
import           Network.Wai.Handler.Warp

import Fission.Internal.Orphanage.RIO ()
import Fission.Internal.UTF8
import Fission.Storage.PostgreSQL     (connPool)

import qualified Fission.Web     as Web
import qualified Fission.Web.Log as Web.Log

import Fission.Internal.Development
-- import Rapid
import System.Remote.Monitoring.Wai


import Control.Concurrent                   as C (threadDelay)
import Control.Concurrent.Async             as C (race_)
import Network.HTTP.Types
import Network.Wai                          as Wai
import Network.Wai.Handler.Warp             as Wai
import Network.Wai.Middleware.RequestLogger as Wai
import System.Directory                     as D (doesFileExist)
import System.Environment

-- myApp :: Application
-- myApp _req send = send $ responseLBS
--     status200
--     [(hContentType, "text/html; charset=utf-8")]
--     "<p>Well, this is really <b>boring</b>.</p>"

-- prodMain :: IO ()
-- prodMain = do
--     putStrLn "Running in production mode on port 8080"
--     run 8080 $ logStdout myApp

main :: IO ()
main = C.race_ watchTermFile $ do
    port <- fmap read $ getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putStrLn $ "Running in development mode on port " ++ show (port :: Int)
    putStrLn $ "But you should connect to port " ++ displayPort
    logOptions <- logOptionsHandle stdout True
    (logFunc, _) <- newLogFunc $ setLogUseTime True logOptions :: IO (LogFunc, IO ())
    dbVar <- runSimpleApp $ connPool 1 1 3600 pgConnectInfo
    processVar <- mkDefaultProcessContext
    httpVar <- HTTP.newManager HTTP.defaultManagerSettings
    -- logOptions <- logOptionsHandle stdout True
    -- (logFunc, _ :: IO ()) <- newLogFunc $ setLogUseTime True logOptions

    let cfg = mkConfig dbVar processVar httpVar logFunc

    runRIO cfg $ do
        liftIO . runSettings (Web.Log.mkSettings logFunc 8000) =<< Web.app

-- | Would certainly be more efficient to use fsnotify, but this is simpler.
watchTermFile :: IO ()
watchTermFile = loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                C.threadDelay 100000
                loop
