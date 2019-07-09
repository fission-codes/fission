-- | Table creation and migration sequences
module Fission.Storage.SQLite.Migrate
  ( Mutation
  , mutations
  ) where

import RIO

import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn
import           Fission.Storage.SQLite        (makeTable)
import           Fission.Storage.Types         as DB
import qualified Fission.User                  as User
import qualified Fission.User.CID              as UserCID

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
  [ makeTable db Heroku.AddOn.addOns Heroku.AddOn.tableName
  , makeTable db User.users          User.tableName
  , makeTable db UserCID.userCIDs    UserCID.tableName
  ]
