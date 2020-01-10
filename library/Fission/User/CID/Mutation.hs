-- | Database mutations for 'UserCid's
module Fission.User.CID.Mutation
  ( create
  , createX
  -- , destroyAll
  -- , destroyExact
  , MonadDBMutation(..)
  ) where

import Database.Esqueleto
import RIO.List ((\\))
import Network.IPFS.CID.Types as IPFS.CID

import Fission.Prelude
import Fission.Models
import Fission.Types

-- | Create a new, timestamped entry
create ::
  ( MonadTime   m
  , MonadDB     m
  )
  => UserId
  -> CID
  -> m (Maybe UserCidId)
create userId cid = runDBNow \now -> do
  insertUnique UserCid
    { userCidUserFk     = userId
    , userCidCid        = cid
    , userCidInsertedAt = now
    , userCidModifiedAt = now
    }

-- | Create new 'UserCid's, ignoring existing values (set-like)
createX ::
  ( MonadDB     m
  , MonadTime   m
  )
  => UserId
  -> [CID]
  -> m [CID]
createX userId hashes = runDBNow \now -> do
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

class MonadDB m => MonadDBMutation m where
  destroyExact :: UserId -> CID -> Transaction m ()
  destroyAll   :: [Key UserCid] -> Transaction m ()

instance MonadDBMutation Fission where

  -- destroyAll :: MonadDB m => [Key UserCid] -> Transaction m ()
  destroyAll userCidIds =
    delete <| from \userCid -> where_ (userCid ^. UserCidId `in_` valList userCidIds)

  -- destroyExact :: MonadDB m => UserId -> CID -> Transaction m ()
  destroyExact userId cid = do
      delete <| from \userCid ->
        where_ (selectExact userCid)
    where
      selectExact userCid =
            userCid ^. UserCidCid    ==. val cid
        &&. userCid ^. UserCidUserFk ==. val userId
