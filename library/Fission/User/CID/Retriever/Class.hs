module Fission.User.CID.Retriever.Class (Retriever (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

class Monad m => Retriever m where
  -- | CIDs associated with a user
  getByUserId :: UserId -> m [Entity UserCid]

  -- | Find all CIDs that remain from a list
  getByCids :: [CID] -> m [Entity UserCid]

instance MonadIO m => Retriever (Transaction m) where
  getByUserId userId =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidUserFk ==. val userId)
      return userCid

  getByCids cids =
    select <| from \userCid -> do
      where_ (userCid ^. UserCidCid `in_` valList cids)
      return userCid
