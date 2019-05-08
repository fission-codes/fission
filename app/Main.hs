{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import System.Remote.Monitoring.Wai

import qualified Fission.Ambient as Ambient
import qualified Fission.Env     as Env
import qualified Fission.Web     as Web

main :: IO ()
main = do
  -- TODO only run locally in dev
  ekgHost <- Ambient.lookup "EKG_HOST" "localhost"
  ekgPort <- Ambient.lookup "EKG_PORT" 8888

  void $ forkServer ekgHost ekgPort

  port <- Ambient.lookup "PORT" 8000

  withStdoutLogger $ \logger -> do
    let portSettings = setPort port
        logSettings  = setLogger logger
        settings     = portSettings $ logSettings defaultSettings
        env          = Env.base

    runRIO env $ do
      logInfo $ "Servant running at port " <> display port <> "\n"
      logInfo $ "EKG available at " <> displayBytesUtf8 ekgHost <> ":" <> display ekgPort <> "\n"
      liftIO $ runSettings settings $ Web.app env
