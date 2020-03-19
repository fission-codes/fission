module Fission.LoosePin.Retriever.Class (Retriever (..)) where

import           Network.IPFS.CID.Types
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

class Monad m => Retriever m where
  -- | CIDs associated with a user
  getByUserId :: UserId -> m [Entity LoosePin]

  -- | Find all CIDs that remain from a list
  getByCids :: [CID] -> m [Entity LoosePin]

instance MonadIO m => Retriever (Transaction m) where
  getByUserId userId =
    select <| from \userCid -> do
      where_ (userCid ^. LoosePinOwnerId ==. val userId)
      return userCid

  getByCids cids =
    select <| from \userCid -> do
      where_ (userCid ^. LoosePinCid `in_` valList cids)
      return userCid
