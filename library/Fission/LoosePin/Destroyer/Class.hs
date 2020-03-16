-- | Database mutations for 'LoosePin's
module Fission.LoosePin.Destroyer.Class (Destroyer (..)) where

import Database.Esqueleto
import Network.IPFS.CID.Types as IPFS.CID

import Fission.Prelude
import Fission.Models

-- | Actions for destroying @LoosePin@s
class Monad m => Destroyer m where
  -- | Destroy a specific @LoosePin@
  destroy :: UserId -> CID -> m ()

  -- | Destroy several @LoosePin@s by they primary keys
  destroyMany :: UserId -> [LoosePinId] -> m ()

instance MonadIO m => Destroyer (Transaction m) where
  destroyMany userId userCidIds =
    delete <| from \pin ->
      where_ <| pin ^. LoosePinId `in_` valList userCidIds
            &&. pin ^. LoosePinOwnerId ==. val userId

  destroy userId cid =
    delete <| from \pin ->
      where_ <| pin ^. LoosePinCid     ==. val cid
            &&. pin ^. LoosePinOwnerId ==. val userId
