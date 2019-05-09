{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

import Network.Wai.Handler.Warp
import Network.Wai.Logger

import qualified Fission.Env        as Env
import qualified Fission.Log        as Log
import qualified Fission.Monitor    as Monitor
import qualified Fission.Web        as Web
import qualified Fission.Web.Config as Web.Config

main :: IO ()
main = runRIO (mkLogFunc Log.simple) $ do
  Web.Config.Config port <- Web.Config.get

  liftIO . withStdoutLogger $ \logger -> do
    let portSettings = setPort port
        logSettings  = setLogger logger
        settings     = portSettings $ logSettings defaultSettings
        env          = Env.base

    runRIO env $ do
      Monitor.wai -- TODO only run locally in dev
      logInfo $ "Servant running at port " <> display port <> "\n"
      liftIO $ runSettings settings $ Web.app env
