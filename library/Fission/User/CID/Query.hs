module Fission.User.CID.Query
  ( getUserCidsByUserId
  , getUserCidsByCids
  ) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

-- | CIDs associated with a user
getUserCidsByUserId :: MonadDB m => UserId -> Transaction m [Entity UserCid]
getUserCidsByUserId userId =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidUserFk ==. val userId)
    return userCid

-- | Find all CIDs that remain from a list
getUserCidsByCids :: MonadDB m => [CID] -> Transaction m [Entity UserCid]
getUserCidsByCids cids =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidCid `in_` valList cids)
    return userCid
