module Fission.Storage.SQLite.Migrate
  ( Mutation
  , mutations
  ) where

import RIO

import           Fission.Config                (DBPath)
import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn
import           Fission.Storage.SQLite        (makeTable)
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
--  NB To run after a certain point: `sequence_ . drop n`
mutations :: DBPath -> [Mutation]
mutations db =
  [ makeTable db Heroku.AddOn.addOns Heroku.AddOn.tableName
  , makeTable db User.users          User.tableName
  ]
