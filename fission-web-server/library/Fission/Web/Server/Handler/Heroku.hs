module Fission.Web.Server.Handler.Heroku (handler) where

import qualified Data.Text                                       as Text
import           Data.UUID                                       as UUID

import           RIO.List                                        ((\\))
import qualified RIO.NonEmpty                                    as NonEmpty

import           Database.Persist.Sql

import           Network.IPFS
import           Network.IPFS.CID.Types
import qualified Network.IPFS.Pin                                as IPFS.Pin

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Random                                  as Random
import           Fission.Security.Types                          (Secret (..))
import           Fission.User.Password.Types
import qualified Fission.User.Provision.Types                    as User
import           Fission.User.Username.Types

import           Fission.Platform.Heroku.Provision.Request.Types
import           Fission.Platform.Heroku.Provision.Types

import           Fission.Web.API.Heroku.Auth.Types               as Heroku
import qualified Fission.Web.API.Heroku.Types                    as Heroku
import qualified Fission.Web.API.Host.Types                      as Web

import qualified Fission.Web.Server.Error                        as Web.Err
import qualified Fission.Web.Server.Heroku.AddOn                 as Heroku.AddOn
import           Fission.Web.Server.IPFS.Linked
import qualified Fission.Web.Server.LoosePin                     as LoosePin
import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB
import           Fission.Web.Server.Reflective
import qualified Fission.Web.Server.User                         as User

handler ::
  ( MonadReflectiveServer    m
  , MonadRemoteIPFS          m
  , MonadLinkedIPFS          m
  , MonadLogger              m
  , MonadThrow               m
  , User.Creator             m
  , MonadDB                t m
  , MonadLogger            t
  , MonadThrow             t
  , User.Retriever         t
  , User.Destroyer         t
  , LoosePin.Retriever     t
  , LoosePin.Destroyer     t
  , Heroku.AddOn.Destroyer t
  , Heroku.AddOn.Retriever t
  )
  => Heroku.Routes (AsServerT m)
handler = Heroku.Routes {..}
  where
    deprovision uuid' Heroku.Auth {} = do
      toUnpin <- runDB $ deleteAssociatedWith uuid'

      forM_ toUnpin \cid ->
        IPFS.Pin.rm cid >>= \case
          Left err -> logWarn $ displayLazyBS err
          Right _  -> pure ()

      return NoContent

    provision Request {uuid, region} _ = do
      username      <- Web.Err.ensure . mkUsername . Text.pack $ UUID.toString uuid
      now           <- getCurrentTime
      secret        <- Random.alphaNum 50
      userID        <- Web.Err.ensureM $ User.createWithHeroku uuid region username (Password secret) now
      Web.Host url' <- getHost
      ipfsPeers     <- getLinkedPeers

      return Provision
        { id      = fromIntegral $ fromSqlKey userID
        , peers   = NonEmpty.toList ipfsPeers
        , message = "Successfully provisioned Interplanetary Fission!"
        , config  = User.Provision
          { username = username
          , password = Secret secret
          , url      = url'
          }
        }

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
