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

import qualified Fission.User                  as User
import qualified Fission.User.CID              as User.CID
import qualified Fission.Platform.Heroku.AddOn as Heroku.AddOn

import qualified Fission.Web.Heroku.MIME.VendorJSONv3.Types as Heroku

type API = Capture "addon_id" UUID
        :> DeleteNoContent '[Heroku.VendorJSONv3] NoContent

destroy ::
  ( MonadDB         m
  , MonadLogger     m
  , MonadThrow      m
  , MonadRemoteIPFS m
  )
  => ServerT API m
destroy uuid' = do
  toUnpin <- runDB <| deleteAssociatedWith uuid'

  forM_ toUnpin \cid ->
    IPFS.Pin.rm cid >>= \case
      Left err -> logError (show err)
      Right _  -> pure ()

  return NoContent

-- | Delete all records associated with a Heroku UUID
deleteAssociatedWith ::
  ( User.Retriever         m
  , User.Destroyer         m
  , User.CID.Retriever     m
  , User.CID.Destroyer     m
  , Heroku.AddOn.Retriever m
  , Heroku.AddOn.Destroyer m
  , MonadLogger            m
  , MonadThrow             m
  )
  => UUID
  -> m [CID]
deleteAssociatedWith uuid' = do
  addOn     <- herokuAddOnByUUID uuid'
  addOnUser <- userForHerokuAddOn (entityKey addOn)
  userCids  <- User.CID.getByUserId (entityKey addOnUser)

  deleteAssociatedRecords (entityKey addOnUser) uuid' userCids

  let deletedUserCids = getInner userCidCid <$> userCids
  remainingUserCids <- User.CID.getByCids deletedUserCids

  let remainingCIDs = getInner userCidCid <$> remainingUserCids
  return (deletedUserCids \\ remainingCIDs)

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
deleteAssociatedRecords ::
  ( User.Destroyer         m
  , User.CID.Destroyer     m
  , Heroku.AddOn.Destroyer m
  )
  => UserId
  -> UUID
  -> [Entity UserCid]
  -> m ()
deleteAssociatedRecords userId uuid userCids = do
  User.CID.destroyX (entityKey <$> userCids)
  User.destroy userId
  Heroku.AddOn.destroyByUUID uuid

-- | Get the User associated with those Heroku add-ons, throw 410 if not found.
userForHerokuAddOn ::
  ( MonadLogger    m
  , MonadThrow     m
  , User.Retriever m
  )
  => HerokuAddOnId
  -> m (Entity User)
userForHerokuAddOn addOnId =
  addOnId
    |> User.getByHerkouAddOnId
    |> ensureEntityM err410

-- | Get a Heroku add-on with a specific UUID, throw 410 if not found.
herokuAddOnByUUID ::
  ( MonadLogger            m
  , MonadThrow             m
  , Heroku.AddOn.Retriever m
  )
  => UUID
  -> m (Entity HerokuAddOn)
herokuAddOnByUUID uuid' =
  uuid'
    |> Heroku.AddOn.getByUUID
    |> ensureEntityM err410
