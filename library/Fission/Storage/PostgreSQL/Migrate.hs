-- | Table creation and migration sequences
module Fission.Storage.PostgreSQL.Migrate where
  -- ( Mutation
  -- , mutations
  -- ) where

import RIO

import Database.Selda.PostgreSQL

import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn.Table
-- import           Fission.Storage.PostgreSQL    (makeTable)
import qualified Fission.User.CID.Table as UserCID.Table
import qualified Fission.User.Table     as User.Table

-- -- | Table creation or migration
-- type Mutation = IO ()

-- -- TODO -- Can express migrations & creation as a DAG
-- -- -- Use `dag: Compile-time, type-safe directed acyclic graphs.`
-- -- data Mutation' m
-- --   = Alone m
-- --   | Dependancy m [m]

-- -- | All migrations, in order
-- --
-- --  NB To run after a certain point: `sequence_ . drop n`
-- --  This oughta be refactored but I couldn't get it to work
-- mutations :: Text
--   -> Int
--   -> Text
--   -> Maybe Text
--   -> Maybe Text
--   -> Maybe Text
--   -> [Mutation]
-- mutations h p d s u pass =  mutations' $ PGConnectInfo h p d s u pass

-- mutations' :: PGConnectInfo -> [Mutation]
-- mutations' db =
--   [ makeTable db Heroku.AddOn.Table.addOns Heroku.AddOn.Table.name
--   , makeTable db User.Table.users          User.Table.name
--   , makeTable db UserCID.Table.userCIDs    UserCID.Table.name
--   ]
