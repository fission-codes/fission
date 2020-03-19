{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RIO
import RIO.Directory
import RIO.Partial   (read)
import RIO.Text      as Text

import Network.Wai.Handler.Warp     (runSettings)
import System.Environment           (getEnv)

import           Fission.Internal.Development
import           Fission.Internal.UTF8
import qualified Fission.Web                  as Web
import qualified Fission.Web.Log              as Web.Log


main :: IO ()
main = race_ watchTermFile $ do
    port         <- read <$> getEnv "PORT"
    displayPort  <- getEnv "DISPLAY_PORT"
    logOptions   <- logOptionsHandle stdout True
    (logFunc, _) <- newLogFunc $ setLogUseTime True logOptions :: IO (LogFunc, IO ())

    putText $ "🔄 Running reloader on port " <> Text.pack (show port) <> "\n"
    putText $ "🚀 Web server up at port " <> Text.pack displayPort <> "\n"
    putText "📈 EKG live at http://localhost:9630\n"

    void $ forkServer "localhost" 9630
    runOne $ do
      server <- Web.app
      liftIO $ runSettings (Web.Log.mkSettings logFunc port) server

-- | TODO move to FSNotify
watchTermFile :: IO ()
watchTermFile = loop
  where
    loop :: IO ()
    loop = do
      exists <- doesFileExist "yesod-devel/devel-terminate"
      unless exists do
        threadDelay 100000
        loop
