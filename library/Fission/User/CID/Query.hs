module Fission.User.CID.Query
  ( getByUserId
  , getByCids
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update, getBy)

import           Fission.Models
import           Fission.Prelude hiding (Value)
import           Fission.Storage

-- | CIDs associated with a user
getByUserId :: MonadDBQuery UserCid m => UserId -> Transaction m [Entity UserCid]
getByUserId userId = getBy (\userCid -> userCid ^. UserCidUserFk ==. val userId)

-- | Find all CIDs that remain from a list
getByCids :: MonadDBQuery UserCid m => [CID] -> Transaction m [Entity UserCid]
getByCids cids = getBy (\userCid -> userCid ^. UserCidCid `in_` valList cids)

