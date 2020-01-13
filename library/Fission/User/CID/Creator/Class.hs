-- | Database mutations for 'UserCid's
module Fission.User.CID.Creator.Class (Creator (..)) where

import           Database.Esqueleto
import           RIO.List ((\\))
import           Network.IPFS.CID.Types as IPFS.CID

import           Fission.Prelude
import           Fission.Models

-- | Actions for creating new @UserCid@s
class Monad m => Creator m where
  -- | Create a new, timestamped entry
  create :: UserId -> CID -> UTCTime -> m (Maybe UserCidId)

  -- | Create new 'UserCid's, ignoring existing values (set-like)
  createX :: UserId -> [CID] -> UTCTime -> m [CID]

instance MonadIO m => Creator (Transaction m) where
  create :: UserId -> CID -> UTCTime -> Transaction m (Maybe UserCidId)
  create userId cid now =
    insertUnique UserCid
      { userCidUserFk     = userId
      , userCidCid        = cid
      , userCidInsertedAt = now
      , userCidModifiedAt = now
      }

  createX :: UserId -> [CID] -> UTCTime -> Transaction m [CID]
  createX userId hashes now = do
    existingCIDs <- select <| from \userCid -> do
      where_ (userCid ^. UserCidCid `in_` valList hashes)
      return (userCid ^. UserCidCid)

    let
      mkFresh :: CID -> UserCid
      mkFresh cid = UserCid
        { userCidUserFk     = userId
        , userCidCid        = cid
        , userCidInsertedAt = now
        , userCidModifiedAt = now
        }

      existingRawCIDs = unValue <$> existingCIDs
      newHashes       = hashes \\ existingRawCIDs
      toInsert        = mkFresh <$> newHashes

    insertMany_ toInsert
    return newHashes
