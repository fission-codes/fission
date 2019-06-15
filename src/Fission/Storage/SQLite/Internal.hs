{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Storage.SQLite.Internal (traceAll) where

import RIO

import Database.Selda
import Database.Selda.SQLite

#ifndef __HLINT__
-- HLint can't handle BlockArguments _yet_
traceAll :: (Show a, Relational a) => Table a -> IO ()
traceAll tbl = withSQLite "ipfs-api.sqlite" do
  rows <- query (select tbl)
  forM_ rows (traceIO . textDisplay . displayShow)
#endif
