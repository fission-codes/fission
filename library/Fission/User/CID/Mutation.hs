-- | Database mutations for 'UserCid's
module Fission.User.CID.Mutation (Insertable (..), Mutable (..)) where

import Database.Esqueleto
import RIO.List ((\\))
import Network.IPFS.CID.Types as IPFS.CID

import Fission.Prelude
import Fission.Models

class Insertable m where
  -- | Create a new, timestamped entry
  create :: UserId -> CID -> UTCTime -> m (Maybe UserCidId)

  -- | Create new 'UserCid's, ignoring existing values (set-like)
  createX :: UserId -> [CID] -> UTCTime -> m [CID]

instance MonadIO m => Insertable (Transaction m) where
  create :: UserId -> CID -> UTCTime -> Transaction m (Maybe UserCidId)
  create userId cid now = Transaction do
    insertUnique UserCid
      { userCidUserFk     = userId
      , userCidCid        = cid
      , userCidInsertedAt = now
      , userCidModifiedAt = now
      }

  createX :: UserId -> [CID] -> UTCTime -> Transaction m [CID]
  createX userId hashes now = Transaction do
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

class Mutable m where
  destroyAll   :: [Key UserCid] -> m ()
  destroyExact :: UserId -> CID -> m ()

instance MonadIO m => Mutable (Transaction m) where
  destroyAll :: [Key UserCid] -> Transaction m ()
  destroyAll userCidIds = Transaction <|
    delete <| from \userCid -> where_ (userCid ^. UserCidId `in_` valList userCidIds)

  destroyExact :: UserId -> CID -> Transaction m ()
  destroyExact userId cid =
    Transaction <| delete <| from \userCid ->
      where_ (selectExact userCid)
    where
      selectExact userCid =
            userCid ^. UserCidCid    ==. val cid
        &&. userCid ^. UserCidUserFk ==. val userId
