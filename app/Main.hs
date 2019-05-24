{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RIO

import Network.Wai.Handler.Warp
import Network.Wai.Logger

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import Fission.Internal.Orphanage

import qualified Fission.Log        as Log
import qualified Fission.Monitor    as Monitor
import qualified Fission.Web        as Web
import           Fission.Web.Config as Web.Config

main :: IO ()
main = withStdoutLogger $ \stdOut ->
  runRIO (mkLogFunc Log.simple) $ do
    Web.Config.Config {port} <- Web.Config.get
    pool <- SQLite.connPool $ DBPath "fission.sqlite"

    let portSettings = setPort port
        logSettings  = setLogger stdOut
        settings     = portSettings $ logSettings defaultSettings
        cfg          = Config.base $ DBPool pool

    runRIO cfg $ do
      Monitor.wai -- TODO only run locally in dev
      logInfo $ "Servant running at port " <> display port
      liftIO $ runSettings settings $ Web.app cfg
