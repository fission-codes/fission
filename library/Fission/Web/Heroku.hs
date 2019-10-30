{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.Heroku
  ( API
  , server
  ) where

import           RIO
import           RIO.Process (HasProcessContext)

import           Data.Has
import           Data.UUID
import           Database.Selda as Selda

import qualified Network.HTTP.Client as HTTP
import           Servant

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

import           Fission.IPFS.CID.Types

import qualified Fission.User.CID       as UserCID
import qualified Fission.User.CID.Table as Table

import qualified Fission.Platform.Heroku.AddOn       as AddOn
import qualified Fission.Platform.Heroku.AddOn.Table as Table
import           Fission.Platform.Heroku.AddOn.Types

import           Fission.Security.Types (Secret (..))

import           Fission.IPFS.Types          as IPFS
import           Fission.Storage.IPFS.Pin as IPFS.Pin
import           Fission.IPFS.Peer (getExternalAddress)

type API = ProvisionAPI :<|> DeprovisionAPI

type ProvisionAPI = ReqBody '[JSON]                     Provision.Request
                 :> Post    '[Heroku.MIME.VendorJSONv3] Provision

server :: HasLogFunc        cfg
       => Has Web.Host      cfg
       => Has HTTP.Manager  cfg
       => Has IPFS.URL      cfg
       => HasProcessContext cfg
       => Has IPFS.BinPath cfg
       => Has IPFS.Timeout cfg
       => MonadSelda   (RIO cfg)
       => RIOServer         cfg API
server = provision :<|> deprovision

provision :: HasLogFunc      cfg
          => Has Web.Host    cfg
          => HasProcessContext cfg
          => Has IPFS.BinPath cfg
          => Has IPFS.Timeout cfg
          => MonadSelda (RIO cfg)
          => RIOServer       cfg ProvisionAPI
provision Request {_uuid, _region} = do
  Web.Host url <- Config.get
  ipfsPeers    <- getExternalAddress >>= \case
                   Right peers' ->
                     pure peers'
                   Left err -> do
                     logError $ displayShow err
                     return []

  username     <- liftIO $ User.genID
  secret       <- liftIO $ Random.text 200
  User.createWithHeroku _uuid _region username secret >>= \case
    Left err -> Web.Err.throw err
    Right userID -> do
      logInfo $ mconcat
        [ "Provisioned UUID: "
        , displayShow _uuid
        , " as "
        , displayShow userID
        ]

      let
        userConfig = User.Provision
          { _url      = url
          , _username = User.hashID userID
          , _password = Secret secret
          }

      return Provision
        { _id      = userID
        , _config  = userConfig
        , _peers   = ipfsPeers
        , _message = "Successfully provisioned Interplanetary Fission!"
        }

type DeprovisionAPI = Capture "addon_id" UUID
                   :> DeleteNoContent '[Heroku.MIME.VendorJSONv3] NoContent

deprovision :: MonadSelda   (RIO cfg)
            => HasLogFunc        cfg
            => Has HTTP.Manager  cfg
            => Has IPFS.URL      cfg
            => RIOServer         cfg DeprovisionAPI
deprovision uuid' = do
  let err = Web.Err.ensureMaybe err410 -- HTTP 410 is specified by the Heroku AddOn docs

  AddOn {_addOnID} <- err =<< Query.oneEq Table.addOns AddOn.uuid' uuid'
  User  {_userID}  <- err =<< Query.findOne do
    user <- select Table.users
    restrict $ user ! #_herokuAddOnId .== literal (Just _addOnID)
           .&& user ! #_active        .== true
    return user

  usersCIDs <- query do
    uCIDs <- select Table.userCIDs
    restrict (uCIDs ! #_userFK .== literal _userID)
    return (uCIDs ! UserCID.cid')

  cidOccur <- query do
    (liveCID' :*: occurences') <- aggregate do
      uCIDs <- select Table.userCIDs
      theCID <- groupBy (uCIDs ! UserCID.cid')
      return (theCID :*: count (uCIDs ! UserCID.cid'))

    restrict (liveCID' `isIn` fmap literal usersCIDs)
    return (liveCID' :*: occurences')

  transaction do
    deleteFrom_ Table.userCIDs (UserCID.userFK' `is` _userID)
    deleteFrom_ Table.users    (User.userID'    `is` _userID)
    deleteFrom_ Table.addOns   (AddOn.uuid'     `is` uuid')

  let toUnpin = CID . Selda.first <$> filter ((== 1) . Selda.second) cidOccur
  forM_ toUnpin $ IPFS.Pin.rm >=> \case
    Left ipfsMsg -> do
      logError $ "Unable to unpin CID: " <> display ipfsMsg
      return ()

    Right _ ->
      return ()

  return NoContent
