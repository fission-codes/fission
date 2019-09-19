{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Fission.Storage.PostgreSQL.Internal () where
-- module Fission.Storage.PostgreSQL.Internal (traceTable) where

-- import RIO

-- import Database.Selda
-- import Database.Selda.PostgreSQL

-- traceTable :: (Show a, Relational a) => Table a -> IO ()
-- traceTable tbl = withPostgreSQL "web-api.sqlite" do
--   rows <- query (select tbl)
--   forM_ rows (traceIO . textDisplay . displayShow)
