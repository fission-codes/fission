{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Storage.SQLite.Migrate (makeTable) where

import RIO

import Fission.Storage.SQLite

import Database.Selda

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import qualified Fission.Log as Log

makeTable :: Table b -> TableName -> IO ()
makeTable tbl name = runRIO (mkLogFunc Log.simple) $ do
  pool <- SQLite.connPool $ DBPath "fission.sqlite"

  runRIO (Config.base (DBPool pool)) $
    setupTable tbl name
