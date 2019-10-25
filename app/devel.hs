{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import RIO
import RIO.Directory
import RIO.Text      as Text

import Network.Wai.Handler.Warp

import Fission.Internal.Orphanage.RIO ()
import Fission.Internal.UTF8

import qualified Fission.Web     as Web
import qualified Fission.Web.Log as Web.Log

import Fission.Internal.Development
import System.Remote.Monitoring.Wai

-- import System.Directory   as D (doesFileExist)
import System.Environment

main :: IO ()
main = race_ watchTermFile $ do
    port <- fmap read $ getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"

    logOptions <- logOptionsHandle stdout True
    (logFunc, _) <- newLogFunc $ setLogUseTime True logOptions :: IO (LogFunc, IO ())

    void $ forkServer "localhost" 9630
    putText $ "🔄 Running reloader on port " <> Text.pack (show port) <> "\n"
    putText $ "🚀 Web server up at port" <> Text.pack displayPort <> "\n"
    putText "📈 EKG live at http://localhost:9630\n"

    runOne $ do
      server <- Web.app
      liftIO $ runSettings (Web.Log.mkSettings logFunc port) server

-- | TODO move to FSNotify
watchTermFile :: IO ()
watchTermFile = loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop
