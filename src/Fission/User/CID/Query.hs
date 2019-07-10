-- | 'User.CID' DB mutations
module Fission.User.CID.Mutation where

import RIO

import Database.Selda as Selda

import Fission.User
import Fission.User.CID as User.CID

inUserCIDs :: Functor  collection
          => Selda.Set collection
          => ID User
          -> collection Text
          -> Query s (Col s Text)
inUserCIDs uID hashes = do
  uCIDs <- select userCIDs
  restrict $ uCIDs `byUser` uID
         .&& uCIDs `preexistingHashes` hashes
  return $ uCIDs ! #_cid

-- byUser ::
uCIDs `byUser` uID = uCIDs ! #_userFK .== literal uID

preexistingHashes :: Functor collection => Selda.Set collection => Row s t -> collection Text -> Col s Bool
preexistingHashes uCIDs hashes = uCIDs ! #_cid `isIn` (text <$> hashes)
