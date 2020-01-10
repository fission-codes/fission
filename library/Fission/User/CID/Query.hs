module Fission.User.CID.Query (MonadDBQuery(..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

class MonadIO m => MonadDBQuery m where
  -- | CIDs associated with a user
  getByUserId :: UserId -> Transaction m [Entity UserCid]
  getByUserId userId =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidUserFk ==. val userId)
      return userCid

  -- | Find all CIDs that remain from a list
  getByCids :: [CID] -> Transaction m [Entity UserCid]
  getByCids cids =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidCid `in_` valList cids)
      return userCid
