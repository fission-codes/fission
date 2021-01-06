module Fission.Web.Server.Handler.Heroku.Deprovision (destroy) where

import           RIO.List                                          ((\\))

import           Data.UUID
import           Database.Esqueleto

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Pin                                  as IPFS.Pin

import           Servant

import           Fission.Prelude

import qualified Fission.Web.Server.LoosePin                       as LoosePin
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.User                           as User

import qualified Fission.Web.Server.Heroku.AddOn                   as Heroku.AddOn
import qualified Fission.Web.Server.Heroku.MIME.VendorJSONv3.Types as Heroku

destroy ::
  ( MonadRemoteIPFS          m
  , MonadLogger              m
  , MonadDB                t m
  , MonadLogger            t
  , MonadThrow             t
  , User.Retriever         t
  , User.Destroyer         t
  , LoosePin.Retriever     t
  , LoosePin.Destroyer     t
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
  , LoosePin.Retriever     t
  , LoosePin.Destroyer     t
  , Heroku.AddOn.Retriever t
  , Heroku.AddOn.Destroyer t
  )
  => UUID
  -> t [CID]
deleteAssociatedWith uuid' = do
  Entity addOnId _ <- ensureEntityM err410 $ Heroku.AddOn.getByUUID uuid'
  Entity userId  _ <- ensureEntityM err410 $ User.getByHerokuAddOnId addOnId
  loosePins        <- LoosePin.getByUserId userId

  deleteAssociatedRecords userId uuid' loosePins

  let deletedUserCids = getInner loosePinCid <$> loosePins
  remainingUserCids <- LoosePin.getByCids deletedUserCids

  let remainingCIDs = getInner loosePinCid <$> remainingUserCids
  return (deletedUserCids \\ remainingCIDs)

-- | All records associated with the UUID, across the user, user CID, and Heroku add-on tables
deleteAssociatedRecords ::
  ( User.Destroyer         t
  , LoosePin.Destroyer     t
  , Heroku.AddOn.Destroyer t
  )
  => UserId
  -> UUID
  -> [Entity LoosePin]
  -> t ()
deleteAssociatedRecords userId uuid loosePins = do
  LoosePin.destroyMany userId (entityKey <$> loosePins)
  User.deactivate userId userId
  Heroku.AddOn.destroyByUUID uuid
