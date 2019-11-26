{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.Heroku
  ( API
  , server
  ) where

import           Data.UUID
import           Database.Selda as Selda

import           Servant

import           Fission.Prelude

import qualified Fission.Web.Error       as Web.Err
import qualified Fission.Web.Heroku.MIME as Heroku.MIME
import           Fission.Web.Server
import qualified Fission.Web.Types       as Web

import           Fission.Platform.Heroku.Provision  as Provision

import qualified Fission.Config as Config
import qualified Fission.Random as Random

import qualified Fission.Storage.Query as Query

import           Fission.User.Types
import qualified Fission.User                 as User
import qualified Fission.User.Table           as Table
import qualified Fission.User.Provision.Types as User

import qualified Fission.User.CID.Table as Table

import qualified Fission.Platform.Heroku.AddOn       as AddOn
import qualified Fission.Platform.Heroku.AddOn.Table as Table
import           Fission.Platform.Heroku.AddOn.Types

import           Fission.Security.Types (Secret (..))

import           Network.IPFS
import           Network.IPFS.Types as IPFS
import           Network.IPFS.Pin   as IPFS.Pin
import           Network.IPFS.Peer  (getExternalAddress)

type API = ProvisionAPI :<|> DeprovisionAPI

type ProvisionAPI = ReqBody '[JSON]                     Provision.Request
                 :> Post    '[Heroku.MIME.VendorJSONv3] Provision

server ::
  ( MonadSelda      (RIO cfg)
  , MonadLocalIPFS  (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , MonadMask       (RIO cfg)
  , HasLogFunc           cfg
  , Has Web.Host         cfg
  )
  => RIOServer cfg API
server = provision :<|> deprovision

provision ::
  ( MonadLocalIPFS (RIO cfg)
  , MonadSelda     (RIO cfg)
  , Has Web.Host        cfg
  , HasLogFunc          cfg
  )
  => RIOServer cfg ProvisionAPI
provision Request {uuid, region} = do
  Web.Host url' <- Config.get
  ipfsPeers     <- getExternalAddress >>= \case
                     Right peers' ->
                       pure peers'

                     Left err -> do
                       logError <| displayShow err
                       return []

  username <- liftIO <| User.genID
  secret   <- liftIO <| Random.text 200

  User.createWithHeroku uuid region username secret >>= \case
    Left err ->
      Web.Err.throw err

    Right userID -> do
      logInfo <| mconcat
        [ "Provisioned UUID: "
        , displayShow uuid
        , " as "
        , displayShow userID
        ]

      return Provision
        { id      = userID
        , peers   = ipfsPeers
        , message = "Successfully provisioned Interplanetary Fission!"
        , config  = User.Provision
           { username = User.hashID userID
           , password = Secret secret
           , url      = url'
           }
        }

type DeprovisionAPI = Capture "addon_id" UUID
                   :> DeleteNoContent '[Heroku.MIME.VendorJSONv3] NoContent

deprovision ::
  ( MonadSelda      (RIO cfg)
  , MonadMask       (RIO cfg)
  , MonadRemoteIPFS (RIO cfg)
  , HasLogFunc           cfg
  )
  => RIOServer cfg DeprovisionAPI
deprovision uuid' = do
  let err = Web.Err.ensureMaybe err410 -- HTTP 410 is specified by the Heroku AddOn docs

  AddOn { addOnID } <- err =<< Query.oneEq Table.addOns #uuid uuid'
  User  { userID }  <- err =<< Query.findOne do
    user <- select Table.users
    restrict <| user ! #herokuAddOnId .== literal (Just addOnID)
            .&& user ! #active        .== true
    return user

  usersCIDs <- query do
    uCIDs <- select Table.userCIDs
    restrict (uCIDs ! #userFK .== literal userID)
    return (uCIDs ! #cid)

  cidOccur <- query do
    (liveCID' :*: occurences') <- aggregate do
      uCIDs <- select Table.userCIDs
      theCID <- groupBy (uCIDs ! #cid)
      return (theCID :*: count (uCIDs ! #cid))

    restrict (liveCID' `isIn` fmap literal usersCIDs)
    return <| liveCID' :*: occurences'

  transaction do
    deleteFrom_ Table.userCIDs <| #userFK `is` userID
    deleteFrom_ Table.users    <| #userID `is` userID
    deleteFrom_ Table.addOns   <| #uuid   `is` uuid'

  let toUnpin = CID . Selda.first <$> filter ((== 1) . Selda.second) cidOccur
  forM_ toUnpin <| IPFS.Pin.rm >=> \case
    Left ipfsMsg -> do
      logError <| "Unable to unpin CID: " <> display ipfsMsg
      return ()

    Right _ ->
      return ()

  return NoContent
