{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Storage.SQLite.Internal (traceTable) where

import RIO

import Database.Selda
import Database.Selda.SQLite

traceTable :: (Show a, Relational a) => Table a -> IO ()
traceTable tbl = withSQLite "web-api.sqlite" do
  rows <- query (select tbl)
  forM_ rows (traceIO . textDisplay . displayShow)
