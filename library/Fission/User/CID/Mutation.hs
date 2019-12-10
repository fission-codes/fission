-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation
  ( create
  , createX
  ) where

import RIO.Orphans ()

import Fission.Prelude
import Fission.IPFS.CID.Types as IPFS.CID
import Fission.Timestamp as Timestamp
import Fission.Storage.Query as Query
import Fission.User (User)

import Fission.User.CID.Query
import Fission.User.CID.Types


{-| Create a new, timestamped entry.
-}
create
  :: ( MonadRIO   cfg m
     , HasLogFunc cfg
     , MonadSelda     m
     , MonadMask      m
     )
  => UserId
  -> CID
  -> m ()
create userId (CID hash) = do
  now <- liftIO getCurrentTime

  -- Find existing CID entry
  existingCID <- Query.oneWhere (\asset -> asset ^. cid ==. hash)

  -- Insert if CID entry doesn't exist yet
  case existingCID of
    Just _  ->
      void

    Nothing ->
      Query.insert <| UserCID { cid = hash, userFK = userId }

  -- Explain situation in logs
  logDebug <| case existingCID of
    Just _  ->
      "UserCID already exists for " <> display hash

    Nothing ->
      "Inserted a new UserCID for CID " <> display hash

  -- Fin
  return ()


{-| Create new `UserCID`s, ignoring existing values (set-like).
-}
createX
  :: ( MonadRIO   cfg m
     , MonadSelda     m
     , HasLogFunc cfg
     )
  => UserId
  -> [CID]
  -> m [CID]
createX userId (fmap IPFS.CID.unaddress -> hashes) = do
  now <- liftIO getCurrentTime

  -- Already existing CIDs
  existingAssets <- Query.many (\asset -> do
    where_ (asset ^. cid in_ hashes)
    return (cid asset)
  )

  -- New asset function
  let mkFresh cid = { cid = cid, userFK = userId }
    |> UserCid
    |> Timestamp.add now

  -- Add new assets to the database
  hashes
    |> without existingAssets
    |> map mkFresh
    |> Query.insertMany

  -- Logs
  logDebug <| mconcat
    [ "Created "
    , display (length newHashes)
    , " new UserCID(s): "
    , displayShow newHashes
    ]

  -- Fin
  return (map CID newHashes)
