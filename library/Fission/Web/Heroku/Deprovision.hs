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

import qualified Fission.User.Query        as UserQuery
import qualified Fission.User.Mutation     as UserMutation
import qualified Fission.User.CID.Query    as UserCIDQuery
import qualified Fission.User.CID.Mutation as UserCIDMutation

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
  userCids <- UserCIDQuery.getUserCidsByUserId userId

  deleteAssociatedRecords userId uuid' userCids

  let deletedUserCids = getInner userCidCid <$> userCids
  remainingUserCids <- UserCIDQuery.getUserCidsByCids deletedUserCids

  let remainingCIDs = getInner userCidCid <$> remainingUserCids
  return (deletedUserCids \\ remainingCIDs)

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
deleteAssociatedRecords :: MonadDB m => UserId -> UUID -> [Entity UserCid] -> Transaction m ()
deleteAssociatedRecords userId uuid userCids = do
  UserCIDMutation.destroyAll      (entityKey <$> userCids)
  UserMutation.destroy        userId
  UserMutation.destroyHerokuAddon uuid -- TODO would be nice if this was cascading....

-- | Get the User associated with those Heroku add-ons, throw 410 if not found.
userIdForHerokuAddOn ::
  ( MonadDB     m
  , MonadLogger m
  , MonadThrow  m
  )
  => HerokuAddOnId
  -> Transaction m UserId
userIdForHerokuAddOn addOnId = ensureOneId err410 =<< (UserQuery.getHerkouAddonByUserId addOnId)

-- | Get a Heroku add-on with a specific UUID, throw 410 if not found.
herokuAddOnByUUID ::
  ( MonadDB     m
  , MonadLogger m
  , MonadThrow  m
  )
  => UUID
  -> Transaction m HerokuAddOnId
herokuAddOnByUUID uuid' = ensureOneId err410 =<< (UserQuery.getHerkouAddonByUUID uuid')
