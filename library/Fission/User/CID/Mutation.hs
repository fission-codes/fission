-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation
  ( create
  , createX
  ) where

import Database.Esqueleto
import RIO.List ((\\))
import Network.IPFS.CID.Types as IPFS.CID

import Fission.Prelude
import Fission.Models

-- | Create a new, timestamped entry
create ::
  ( MonadTime   m
  , MonadDB     m
  )
  => UserId
  -> CID
  -> m (Maybe UserCIDId)
create userId cid = runDBNow \now -> do
  insertUnique UserCID
    { userCIDUserFk     = userId
    , userCIDCid        = cid
    , userCIDInsertedAt = now
    , userCIDModifiedAt = now
    }

-- | Create new 'UserCID's, ignoring existing values (set-like)
createX ::
  ( MonadDB     m
  , MonadTime   m
  )
  => UserId
  -> [CID]
  -> m [CID]
createX userId hashes = runDBNow \now -> do
  existingCIDs <- select <| from \userCID -> do
    where_ (userCID ^. UserCIDCid `in_` valList hashes)
    return (userCID ^. UserCIDCid)

  let
    mkFresh :: CID -> UserCID
    mkFresh cid = UserCID
      { userCIDUserFk     = userId
      , userCIDCid        = cid
      , userCIDInsertedAt = now
      , userCIDModifiedAt = now
      }

    existingRawCIDs = unValue <$> existingCIDs
    newHashes       = hashes \\ existingRawCIDs
    toInsert        = mkFresh <$> newHashes

  insertMany_ toInsert
  return newHashes
