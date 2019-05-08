{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO

import qualified Fission.Ambient as Ambient
import qualified Fission.Env     as Env
import qualified Fission.Web     as Web

import Network.Wai.Handler.Warp
import Network.Wai.Logger

main :: IO ()
main = do
  port <- Ambient.lookup "PORT" 8000

  withStdoutLogger $ \logger -> do
    let portSettings = setPort port
        logSettings  = setLogger logger
        settings     = portSettings $ logSettings defaultSettings
        env          = Env.base

    runRIO env $ do
      logInfo $ "Servant running at port " <> display port <> "\n"
      liftIO $ runSettings settings $ Web.app env
