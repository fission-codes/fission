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
  userCIDs <- cidsForUserId userId

  userCIDs
    |> associatedRecords uuid'
    |> delete

  let cids = getInner userCIDCid <$> userCIDs
  remaining <- getRemainingCIDs cids

  let remainingCIDs = getInner userCIDCid <$> remaining
  return (cids \\ remainingCIDs)

-- | Find all CIDs that remain from a list
getRemainingCIDs :: MonadDB m => [CID] -> Transaction m [Entity UserCID]
getRemainingCIDs cids =
  select <| from \userCID -> do
    where_ (userCID ^. UserCIDCid `in_` valList cids)
    return userCID

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
associatedRecords :: UUID -> [Entity UserCID] -> SqlQuery ()
associatedRecords uuid' userCIDs = do
  let userCIDIds = entityKey <$> userCIDs
  from \userCID ->
    where_ (userCID ^. UserCIDId `in_` valList userCIDIds)

  let userIds' = getInner userCIDUserFk <$> userCIDs
  from \user ->
    where_ (user ^. UserId `in_` valList userIds')

  from \herokuAddOn ->
    where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid')

-- | CIDs associated with a user
cidsForUserId :: MonadDB m => UserId -> Transaction m [Entity UserCID]
cidsForUserId userId =
  select <| from \userCID -> do
    where_ (userCID ^. UserCIDUserFk ==. val userId)
    return userCID

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
