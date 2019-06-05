{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Fission.Storage.SQLite.Internal (traceAll) where

import RIO hiding (id)

import Database.Selda
import Database.Selda.SQLite

traceAll :: (Show a, Relational a) => Table a -> IO ()
traceAll tbl = withSQLite "fission.sqlite" $ do
  rows <- query (select tbl)
  forM_ rows (traceIO . textDisplay . displayShow)
