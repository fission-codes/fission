-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation where

import RIO
import RIO.List ((\\))

import Data.Time      (getCurrentTime)
import Database.Selda

import Fission.Internal.Constraint
import Fission.IPFS.CID.Types      as IPFS.CID

import Fission.User           (User)
import Fission.User.CID.Query
import qualified Fission.User.CID.Table as Table
import Fission.User.CID.Types

import Fission.Timestamp as Timestamp

-- | Create a new, timestamped entry
create :: MonadRIO   cfg m
       => HasLogFunc cfg
       => MonadSelda     m
       => ID User
       -> CID
       -> m (ID UserCID)
create userID (CID hash) = do
  now  <- liftIO getCurrentTime
  uCID <- insertWithPK Table.userCIDs [UserCID def userID hash <@ now]

  logInfo $ "Inserted user CID " <> display uCID
  return uCID

-- | Create new 'UserCID's, ignoring existing values (set-like)
createX :: MonadSelda m => ID User -> [CID] -> m Int
createX uID cids = do
  let hashes = IPFS.CID.unaddress <$> cids

  results <- query do
    uCIDs <- select Table.userCIDs
    restrict $ inUserCIDs uID hashes uCIDs
    return $ uCIDs ! #_cid

  now <- liftIO getCurrentTime
  let new = \hash -> Timestamp.add now $ UserCID def uID hash
  insert Table.userCIDs $ new <$> hashes \\ results
