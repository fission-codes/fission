-- | Database mutations for 'LoosePin's
module Fission.LoosePin.Creator.Class (Creator (..)) where

import           Database.Esqueleto
import           RIO.List ((\\))
import           Network.IPFS.CID.Types as IPFS.CID

import           Fission.Prelude
import           Fission.Models

-- | Actions for creating new @LoosePin@s
class Monad m => Creator m where
  -- | Create a new, timestamped entry
  create :: UserId -> CID -> UTCTime -> m (Maybe LoosePinId)

  -- | Create new 'LoosePin's, ignoring existing values (set-like)
  createMany :: UserId -> [CID] -> UTCTime -> m [CID]

instance MonadIO m => Creator (Transaction m) where
  create userId cid now =
    insertUnique LoosePin
      { loosePinOwnerId    = userId
      , loosePinCid        = cid
      , loosePinInsertedAt = now
      }

  createMany userId hashes now = do
    existingCIDs <- select <| from \loosePin -> do
      where_ (loosePin ^. LoosePinCid `in_` valList hashes)
      return (loosePin ^. LoosePinCid)

    let
      mkFresh :: CID -> LoosePin
      mkFresh cid = LoosePin
        { loosePinOwnerId    = userId
        , loosePinCid        = cid
        , loosePinInsertedAt = now
        }

      existingRawCIDs = unValue <$> existingCIDs
      newHashes       = hashes \\ existingRawCIDs
      toInsert        = mkFresh <$> newHashes

    insertMany_ toInsert
    return newHashes
