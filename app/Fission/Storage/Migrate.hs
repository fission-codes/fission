-- | Table creation and migration sequences
module Fission.Storage.Migrate where
--   ( Mutation
--   , TableMaker
--   , mutations
--   ) where

-- import RIO

-- import Database.Selda.PostgreSQL

-- import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn.Table
-- import           Fission.Storage.PostgreSQL    (makeTable)
-- import qualified Fission.User.CID.Table        as UserCID.Table
-- import qualified Fission.User.Table            as User.Table

-- newtype TableMaker = TableMaker { setup :: Table t -> Table.Name t -> IO () }

-- -- | Table creation or migration
-- type Mutation = IO ()

-- mutations :: TableMaker -> [Mutation]
-- mutations TableMaker setup =
--   [ setup Heroku.AddOn.Table.addOns Heroku.AddOn.Table.name
--   , setup User.Table.users          User.Table.name
--   , setup UserCID.Table.userCIDs    UserCID.Table.name
--   ]
