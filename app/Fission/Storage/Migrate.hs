-- | Table creation and migration sequences
module Fission.Storage.Migrate
  ( Mutation
  , mutations
  ) where

import RIO

import Database.Selda.PostgreSQL

import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn.Table
import           Fission.Storage.PostgreSQL    (makeTable)
import qualified Fission.User.CID.Table        as UserCID.Table
import qualified Fission.User.Table            as User.Table

-- | Table creation or migration
type Mutation = IO ()


mutations :: PGConnectInfo -> [Mutation]
mutations db =
  [ makeTable db Heroku.AddOn.Table.addOns Heroku.AddOn.Table.name
  , makeTable db User.Table.users          User.Table.name
  , makeTable db UserCID.Table.userCIDs    UserCID.Table.name
  ]
