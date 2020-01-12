module Fission.User.CID.Query (Queryable (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

class Queryable m where
  -- | CIDs associated with a user
  getByUserId :: UserId -> m [Entity UserCid]

  -- | Find all CIDs that remain from a list
  getByCids :: [CID] -> m [Entity UserCid]

instance MonadIO m => Queryable (Transaction m) where
  getByUserId :: UserId -> Transaction m [Entity UserCid]
  getByUserId userId = Transaction do
    select <| from \userCid -> do
      where_ (userCid ^. UserCidUserFk ==. val userId)
      return userCid

  getByCids :: [CID] -> Transaction m [Entity UserCid]
  getByCids cids = Transaction do
    select <| from \userCid -> do
      where_ (userCid ^. UserCidCid `in_` valList cids)
      return userCid
