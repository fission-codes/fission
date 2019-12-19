module Fission.Web.Heroku.Deprovision
  ( API
  , destroy
  ) where

import           RIO.List ((\\))

import           Data.UUID
import           Database.Esqueleto

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Pin as IPFS.Pin

import           Servant

import           Fission.Prelude
import           Fission.Models

import qualified Fission.Web.Heroku.MIME.VendorJSONv3.Types as Heroku

type API = Capture "addon_id" UUID
        :> DeleteNoContent '[Heroku.VendorJSONv3] NoContent

destroy ::
  ( MonadDB         m
  , MonadThrow      m
  , MonadRemoteIPFS m
  , MonadLogger     m
  , MonadReader cfg m
  )
  => ServerT API m
destroy uuid' = do
  toUnpin <- runDB (deleteAssociatedWith uuid')

  forM_ toUnpin \cid ->
    IPFS.Pin.rm cid >>= \case
      Left err -> logError (show err)
      Right _  -> pure ()

  return NoContent

-- | Delete all records associated with a Heroku UUID
deleteAssociatedWith ::
  ( MonadDB     m
  , MonadLogger m
  , MonadThrow  m
  )
  => UUID
  -> Transaction m [CID]
deleteAssociatedWith uuid' = do
  addOnId  <- herokuAddOnByUUID uuid'
  userId   <- userIdForHerokuAddOn addOnId
  userCids <- cidsForUserId userId

  userCids
    |> associatedRecords uuid'
    |> delete

  let cids = getInner userCidCid <$> userCids
  remaining <- getRemainingCIDs cids

  let remainingCIDs = getInner userCidCid <$> remaining
  return (cids \\ remainingCIDs)

-- | Find all CIDs that remain from a list
getRemainingCIDs :: MonadDB m => [CID] -> Transaction m [Entity UserCid]
getRemainingCIDs cids =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidCid `in_` valList cids)
    return userCid

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
associatedRecords :: UUID -> [Entity UserCid] -> SqlQuery ()
associatedRecords uuid' userCids = do
  let userCidIds = entityKey <$> userCids
  from \userCid ->
    where_ (userCid ^. UserCidId `in_` valList userCidIds)

  let userIds' = getInner userCidUserFk <$> userCids
  from \user ->
    where_ (user ^. UserId `in_` valList userIds')

  from \herokuAddOn ->
    where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid')

-- | CIDs associated with a user
cidsForUserId :: MonadDB m => UserId -> Transaction m [Entity UserCid]
cidsForUserId userId =
  select <| from \userCid -> do
    where_ (userCid ^. UserCidUserFk ==. val userId)
    return userCid

-- | Users associated with those Heroku add-ons
userIdForHerokuAddOn ::
  ( MonadDB     m
  , MonadLogger m
  , MonadThrow  m
  )
  => HerokuAddOnId
  -> Transaction m UserId
userIdForHerokuAddOn addOnId = ensureOneId err410 =<< query
  where
    query = select <| from \user -> do
      where_ <| user ^. UserHerokuAddOnId ==. val (Just addOnId)
            &&. user ^. UserActive        ==. val True
      limit 1
      return user

-- | Heroku add-on with a specific UUID
herokuAddOnByUUID ::
  ( MonadDB     m
  , MonadLogger m
  , MonadThrow  m
  )
  => UUID
  -> Transaction m HerokuAddOnId
herokuAddOnByUUID uuid' = ensureOneId err410 =<< query
  where
    query = select <| from \herokuAddOn -> do
      where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid')
      limit 1
      return herokuAddOn
