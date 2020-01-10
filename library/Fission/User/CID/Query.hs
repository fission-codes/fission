module Fission.User.CID.Query
  ( getByUserId
  , getByCids
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

-- | CIDs associated with a user
getByUserId :: MonadDB m => UserId -> Transaction m [Entity UserCid]
getByUserId userId =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidUserFk ==. val userId)
    return userCid

-- | Find all CIDs that remain from a list
getByCids :: MonadDB m => [CID] -> Transaction m [Entity UserCid]
getByCids cids =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidCid `in_` valList cids)
    return userCid
