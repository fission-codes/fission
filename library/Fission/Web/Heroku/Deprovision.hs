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

import qualified Fission.User as User
import qualified Fission.User.CID as User.CID

import qualified Fission.Web.Heroku.MIME.VendorJSONv3.Types as Heroku

type API = Capture "addon_id" UUID
        :> DeleteNoContent '[Heroku.VendorJSONv3] NoContent

destroy ::
  ( User.MonadDBMutation     m
  -- , User.Queryable        m
  , User.CID.MonadDBMutation m
  , MonadDB                  m
  , MonadLogger              m
  , MonadThrow               m
  , MonadRemoteIPFS          m
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
  ( -- User.MonadDBMutation     m
  User.Queryable        m
  -- , User.CID.MonadDBMutation m
  , MonadLogger              m
  , MonadThrow               m
  -- , MonadIO m
  , User.CID.Queryable    m
  )
  => UUID
  -> m [CID]
deleteAssociatedWith uuid' = do
  addOn     <- herokuAddOnByUUID uuid'
  addOnUser <- userForHerokuAddOn (entityKey addOn)
  userCids  <- User.CID.getByUserId (entityKey addOnUser)

  -- deleteAssociatedRecords (entityKey addOnUser) uuid' userCids

  let deletedUserCids = getInner userCidCid <$> userCids
  remainingUserCids <- User.CID.getByCids deletedUserCids

  let remainingCIDs = getInner userCidCid <$> remainingUserCids
  return (deletedUserCids \\ remainingCIDs)

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
deleteAssociatedRecords ::
  User.MonadDBMutation m
  => User.CID.MonadDBMutation m
  => UserId
  -> UUID
  -> [Entity UserCid]
  -> Transaction m ()
deleteAssociatedRecords userId uuid userCids = do
  User.CID.destroyAll (entityKey <$> userCids)
  User.destroy userId
  User.destroyHerokuAddon uuid

-- | Get the User associated with those Heroku add-ons, throw 410 if not found.
userForHerokuAddOn ::
  ( MonadLogger    m
  , MonadThrow     m
  , User.Queryable m
  )
  => HerokuAddOnId
  -> m (Entity User)
userForHerokuAddOn addOnId =
  addOnId
    |> User.getHerkouAddonByUserId
    |> ensureEntityM err410

-- | Get a Heroku add-on with a specific UUID, throw 410 if not found.
herokuAddOnByUUID ::
  ( MonadLogger    m
  , MonadThrow     m
  , User.Queryable m
  )
  => UUID
  -> m (Entity HerokuAddOn)
herokuAddOnByUUID uuid' =
  uuid'
    |> User.getHerkouAddonByUUID
    |> ensureEntityM err410
