-- | Table creation and migration sequences
module Fission.Storage.SQLite.Migrate
  ( Mutation
  , mutations
  ) where

import RIO

import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn.Table
import           Fission.Storage.SQLite        (makeTable)
import           Fission.Storage.Types         as DB
import qualified Fission.User.CID.Table        as UserCID.Table
import qualified Fission.User.Table            as User.Table

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
mutations :: DB.Path -> [Mutation]
mutations db =
  [ makeTable db Heroku.AddOn.Table.addOns Heroku.AddOn.Table.name
  , makeTable db User.Table.users          User.Table.name
  , makeTable db UserCID.Table.userCIDs    UserCID.Table.name
  ]
