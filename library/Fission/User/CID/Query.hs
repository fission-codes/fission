module Fission.User.CID.Query (MonadDBQuery(..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude
import           Fission.Types

class MonadDB m => MonadDBQuery m where
  getByCids :: [CID] -> Transaction m [Entity UserCid]
  getByUserId :: UserId -> Transaction m [Entity UserCid]

instance MonadDBQuery Fission where

  -- | CIDs associated with a user
  getByUserId userId =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidUserFk ==. val userId)
      return userCid

  -- | Find all CIDs that remain from a list
  getByCids cids =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidCid `in_` valList cids)
      return userCid
