module Fission.Storage.SQLite.Migrate
  ( Mutation
  , makeTable
  , mutations
  ) where

import RIO

import Database.Selda

import Fission.Config         as Config
import Fission.Storage.SQLite as SQLite

import qualified Fission.Log                   as Log
import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn
import qualified Fission.User                  as User

-- | Table creation or migration
type Mutation = IO ()

-- TODO -- Can express migrations & creation as a DAG
-- -- Use `dag: Compile-time, type-safe directed acyclic graphs.`
-- data Mutation' m
--   = Alone m
--   | Dependancy m [m]

-- | All migrations, in order
--
--  NB  To run after a certain point: `sequence_ . drop n`
mutations :: [Mutation]
mutations =
  [ makeTable Heroku.AddOn.addOns Heroku.AddOn.tableName
  , makeTable User.users          User.tableName
  ]

makeTable :: Table t -> TableName' t -> IO ()
makeTable tbl tblName = runRIO (mkLogFunc Log.simple) do
  pool <- SQLite.connPool $ DBPath "ipfs-api.sqlite"
  runRIO (Config.base $ DBPool pool) (setupTable tbl $ unTable tblName)
