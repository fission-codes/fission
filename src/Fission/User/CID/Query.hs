-- | 'User.CID' DB queries
module Fission.User.CID.Query
  ( inUserCIDs
  , eqUserCID
  , byUser
  , byCID
  , inCIDs
  ) where

import RIO

import Database.Selda

import Fission.User.Types
import Fission.User.CID.Types

inUserCIDs :: ID User -> [Text] -> Row s UserCID -> Col s Bool
inUserCIDs uID targetHashes uCIDs =
      uCIDs `byUser` uID
  .&& uCIDs `inCIDs` targetHashes

eqUserCID uID targetHash uCIDs =
      uCIDs `byUser` uID
  .&& uCIDs `byCID` targetHash

row `byUser` uID    = row ! #_userFK .== literal uID
row `byCID`  hash   = row ! #_cid    .== text hash
row `inCIDs` hashes = row ! #_cid `isIn` fmap text hashes
