-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation (insertNewX) where

import RIO
import RIO.List

import Database.Selda

import Fission.User
import Fission.User.CID
import Fission.User.CID.Query
import Fission.IPFS.CID.Types as IPFS.CID
import Fission.Storage.Mutate

-- | Create new 'UserCID's, ignoring existing values (set-like)
insertNewX :: MonadSelda m => ID User -> [CID] -> m (ID UserCID)
insertNewX uID cids = transaction do
  let hashes = IPFS.CID.unaddress <$> cids
  results <- query $ select userCIDs >>= inUserCIDs uID hashes
  insertX' $ UserCID def uID <$> hashes \\ results
