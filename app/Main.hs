{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import RIO

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Envy

import           Fission.Config
import qualified Fission.Log        as Log
import qualified Fission.Monitor    as Monitor
import qualified Fission.Web        as Web
import qualified Fission.Web.Config as Web.Config

main :: IO ()
main = withStdoutLogger $ \stdOut ->
  runRIO (mkLogFunc Log.simple) $ do
    Web.Config.Config {..} <- Web.Config.get

    let portSettings = setPort port
        logSettings  = setLogger stdOut
        settings     = portSettings $ logSettings defaultSettings
        cfg          = (defConfig :: Config)

    runRIO cfg $ do
      Monitor.wai -- TODO only run locally in dev
      logInfo $ "Servant running at port " <> display port
      liftIO $ runSettings settings $ Web.app cfg
