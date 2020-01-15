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
  ( MonadRemoteIPFS          m
  , MonadLogger              m
  , MonadDB                t m
  , MonadLogger            t
  , MonadThrow             t
  , User.Retriever         t
  , User.Destroyer         t
  , User.CID.Retriever     t
  , User.CID.Destroyer     t
  , Heroku.AddOn.Retriever t
  , Heroku.AddOn.Destroyer t
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
  ( MonadThrow             t
  , MonadLogger            t
  , User.Retriever         t
  , User.Destroyer         t
  , User.CID.Retriever     t
  , User.CID.Destroyer     t
  , Heroku.AddOn.Retriever t
  , Heroku.AddOn.Destroyer t
  )
  => UUID
  -> t [CID]
deleteAssociatedWith uuid' = do
  Entity addOnId _ <- ensureEntityM err410 <| Heroku.AddOn.getByUUID uuid'
  Entity userId  _ <- ensureEntityM err410 <| User.getByHerokuAddOnId addOnId
  userCids         <- User.CID.getByUserId userId

  deleteAssociatedRecords userId uuid' userCids

  let deletedUserCids = getInner userCidCid <$> userCids
  remainingUserCids <- User.CID.getByCids deletedUserCids

  let remainingCIDs = getInner userCidCid <$> remainingUserCids
  return (deletedUserCids \\ remainingCIDs)

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
deleteAssociatedRecords ::
  ( User.Destroyer         t
  , User.CID.Destroyer     t
  , Heroku.AddOn.Destroyer t
  )
  => UserId
  -> UUID
  -> [Entity UserCid]
  -> t ()
deleteAssociatedRecords userId uuid userCids = do
  User.CID.destroyMany (entityKey <$> userCids)
  User.destroy userId
  Heroku.AddOn.destroyByUUID uuid
