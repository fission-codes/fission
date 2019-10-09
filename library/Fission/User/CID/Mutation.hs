-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation
  ( create
  , createX
  ) where

import RIO
import RIO.List ((\\))
import RIO.Orphans ()

import Data.Time      (getCurrentTime)
import Database.Selda

import Fission.IPFS.CID.Types      as IPFS.CID
import Fission.Internal.Constraint
import Fission.Timestamp as Timestamp
import Fission.User           (User)

import           Fission.User.CID.Query
import qualified Fission.User.CID.Table as Table
import           Fission.User.CID.Types

-- | Create a new, timestamped entry
create :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadSelda     m
       => MonadMask      m
       => ID User
       -> CID
       -> m (Maybe (ID UserCID))
create userID (CID hash) = do
  now   <- liftIO getCurrentTime
  mayID <- insertUnless Table.userCIDs (eqUserCID userID hash)
            [UserCID def userID hash <@ now]

  logDebug $ case mayID of
    Nothing  -> "UserCID already exists for " <> display hash
    Just _id -> "Inserted a new UserCID for CID " <> display hash

  return mayID

-- | Create new 'UserCID's, ignoring existing values (set-like)
createX :: MonadRIO   cfg m
        => MonadSelda     m
        => HasLogFunc cfg
        => ID User
        -> [CID]
        -> m [CID]
createX uID (fmap IPFS.CID.unaddress -> hashes) = do
  results <- query do
    uCIDs <- select Table.userCIDs `suchThat` inUserCIDs uID hashes
    return $ uCIDs ! #_cid

  now <- liftIO getCurrentTime

  let
    mkFresh   = Timestamp.add now . UserCID def uID
    newHashes = hashes \\ results

  insert Table.userCIDs $ mkFresh <$> newHashes

  logDebug $ mconcat
    [ "Created "
    , display (length newHashes)
    , " new UserCID(s): "
    , displayShow newHashes
    ]

  return $ CID <$> newHashes
