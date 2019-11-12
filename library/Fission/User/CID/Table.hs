module Fission.User.CID.Table
  ( name
  , userCIDs
  ) where

import Database.Selda

import           Fission.User.CID.Types
import qualified Fission.Storage.Table as Table

-- | The name of the 'users' table
name :: Table.Name UserCID
name = "user_cids"

-- | The 'User' table
userCIDs :: Table UserCID
userCIDs = table (Table.name name)
  [ #userCID :- autoPrimary
  , #userFK  :- index
  , #cid     :- index
  ]
