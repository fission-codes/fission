-- | 'User.CID' DB mutations
module Fission.User.CID.Query where
  -- ( inUserCIDs
  -- , byUser
  -- , matchingCIDs
  -- ) where

import RIO

import Database.Selda

import Fission.User
import Fission.User.CID

inUserCIDs uID targetHashes uCIDs = do
  restrict $ uCIDs `byUser` uID
         .&& uCIDs `inCIDs` targetHashes
  return $ uCIDs ! #_cid

eqUserCID uID targetHash uCIDs = do
  restrict $ uCIDs `byUser` uID
         .&& uCIDs `eqCID` targetHash
  return uCIDs -- FIXME Will this work, or just return the initial value?!

row `byUser` uID = row ! #_userFK .== literal uID

row `eqCID`  hash = row ! #_cid .== text hash
row `inCIDs` hashes = row ! #_cid `isIn` (text <$> hashes)
