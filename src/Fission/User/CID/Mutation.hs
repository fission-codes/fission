-- | Database mutations for 'UserCID's
module Fission.User.CID.Mutation
  ( create
  , createX
  ) where

import RIO
import RIO.List ((\\))

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
createX uID (fmap IPFS.CID.unaddress -> hashes) = do
  results <- query do
    match <- select Table.userCIDs `suchThat` inUserCIDs uID hashes
    return $ match ! #_cid

  now <- liftIO getCurrentTime

  let
    mkFresh = Timestamp.add now . UserCID def uID
    newCIDs = hashes \\ results

  insert Table.userCIDs $ mkFresh <$> newCIDs
