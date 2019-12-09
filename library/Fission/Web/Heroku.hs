module Fission.Web.Heroku
  ( API
  , server
  ) where

import           Data.UUID
import           Database.Selda as Selda
import qualified Network.HTTP.Client as HTTP
import           Servant

-- Fission

import qualified Fission.Config as Config
import           Fission.Platform.Heroku.AddOn.Types as Addon
import           Fission.Platform.Heroku.Provision  as Provision
import           Fission.Prelude
import qualified Fission.Random as Random
import           Fission.Security.Types (Secret (..))
import qualified Fission.Storage.Query as Query

-- IPFS

import           Fission.IPFS.Types as IPFS
import           Fission.IPFS.Peer (getExternalAddress)
import           Fission.IPFS.CID.Types as CID
import           Fission.Storage.IPFS.Pin as IPFS.Pin

-- User

import           Fission.User.Types
import qualified Fission.User                 as User
import qualified Fission.User.CID.Types       as UserCID
import qualified Fission.User.Provision.Types as User

-- Web

import qualified Fission.Web.Error       as Web.Err
import qualified Fission.Web.Heroku.MIME as Heroku.MIME
import           Fission.Web.Server
import qualified Fission.Web.Types       as Web


-- APIs


type API = ProvisionAPI :<|> DeprovisionAPI



-- SERVER


server
  :: ( HasLogFunc        cfg
     , Has Web.Host      cfg
     , Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     , HasProcessContext cfg
     , Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , MonadMask    (RIO cfg)
     , MonadSelda   (RIO cfg)
     )
  => RIOServer cfg API
server = provision :<|> deprovision



-- PROVISION


type ProvisionAPI = ReqBody '[JSON]                     Provision.Request
                 :> Post    '[Heroku.MIME.VendorJSONv3] Provision


provision
  :: ( HasLogFunc        cfg
     , Has Web.Host      cfg
     , HasProcessContext cfg
     , Has IPFS.BinPath  cfg
     , Has IPFS.Timeout  cfg
     , MonadSelda   (RIO cfg)
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

  username <- liftIO (User.genID)
  secret   <- liftIO (Random.text 200)

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



-- DEPROVISION


type DeprovisionAPI = Capture "addon_id" UUID
                   :> DeleteNoContent '[Heroku.MIME.VendorJSONv3] NoContent


deprovision
  :: ( MonadSelda   (RIO cfg)
     , MonadMask    (RIO cfg)
     , HasLogFunc        cfg
     , Has HTTP.Manager  cfg
     , Has IPFS.URL      cfg
     )
  => RIOServer cfg DeprovisionAPI
deprovision uuid = do
  -- HTTP 410 is specified by the Heroku AddOn docs
  let err = Web.Err.ensureMaybe err410

  -- Find addOn
  AddOn { addOnId } <- err =<< Query.oneWhere (\addOn ->
    addOn ^. AddOn.uuid ==. uuid
  )

  -- Find user associated with addOn
  User { userId } <- err =<< Query.oneWhere (\user ->
    user ?. User.heroku ==. just addOnID ++
    user ^. User.active ==. True
  )

  -- Find all the CIDs associated with the user
  userCIDs <- (\uc -> uc ^. userFK ==. userId)
    |> Query.manyWhere
    |> map cid

  -- Unpin every CID on IPFS from the user that is not used by another user.
  --
  -- > CIDs can be associated with many users, but in IPFS they point
  --   to exactly one thing. Hence, we cannot not remove that one thing if
  --   there still remains one user using it.
  --
  cidsUsedByOthers <- Query.many (\asset ->
    Query.where_
      ( asset ^. cid in_ userCIDs ++
        asset ^. userFK !=. userId
      )

    asset
      |> cid
      |> return
      |> Query.distinct
  )

  userCIDS
    |> without cidsUsedByOthers
    |> map IPFS.Pin.rm
    |> sequence

  -- Delete everything
  Query.deleteWhere (\uc -> uc ^. UserCID.userFK ==. userId)
  Query.deleteWhere (\us -> us ^. User.userId ==. userId)
  Query.deleteWhere (\ad -> ad ^. AddOn.uuid ==. uuid)

  -- Empty response
  return NoContent
