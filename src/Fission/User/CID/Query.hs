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
         .&& uCIDs `matchingCIDs` targetHashes
  return $ uCIDs ! #_cid

row `byUser` uID = row ! #_userFK .== literal uID

row `matchingCIDs` hashes = row ! #_cid `isIn` (text <$> hashes)
