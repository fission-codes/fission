-- | 'User.CID' DB queries
module Fission.User.CID.Query
  ( inUserCIDs
  , eqUserCID
  , byUser
  , byCID
  , inCIDs
  ) where

import Database.Selda

import Fission.Prelude
import Fission.User.Types
import Fission.User.CID.Types

inUserCIDs :: ID User -> [Text] -> Row s UserCID -> Col s Bool
inUserCIDs uID targetHashes uCIDs =
      uCIDs `byUser` uID
  .&& uCIDs `inCIDs` targetHashes

eqUserCID :: ID User -> Text -> Row s UserCID -> Col s Bool
eqUserCID uID targetHash uCIDs =
      uCIDs `byUser` uID
  .&& uCIDs `byCID` targetHash

byUser :: Row s UserCID -> ID User -> Col s Bool
row `byUser` uID = row ! #userFK .== literal uID

byCID :: Row s UserCID -> Text -> Col s Bool
row `byCID`  hash = row ! #cid .== text hash

inCIDs :: Row s UserCID -> [Text] -> Col s Bool
row `inCIDs` hashes = row ! #cid `isIn` fmap text hashes
