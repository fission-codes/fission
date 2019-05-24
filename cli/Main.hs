{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RIO

import qualified Network.Wai.Cli as CLI

import Fission.Internal.Orphanage

import           Fission.Config         as Config
import qualified Fission.Log            as Log
import qualified Fission.Monitor        as Monitor
import           Fission.Storage.SQLite as SQLite
import qualified Fission.Web            as Web
import           Fission.Web.Config     as Web.Config

main :: IO ()
main = runRIO (mkLogFunc Log.simple) $ do
  Web.Config.Config {port} <- Web.Config.get
  pool <- SQLite.connPool $ DBPath "FIXME.sqlite"

  let cfg = Config.base $ DBPool pool

  runRIO cfg $ do
    Monitor.wai -- TODO only run locally in dev
    logInfo $ "Servant running at port " <> display port
    liftIO . CLI.defWaiMain $ Web.app cfg
