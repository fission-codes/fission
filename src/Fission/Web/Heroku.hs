{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.Heroku
  ( API
  , create
  ) where

import           RIO

import           Data.Has
import           Data.UUID
import           Database.Selda

import           Servant
import           Servant.API

import Network.HTTP.Types

import qualified Fission.Web.Heroku.MIME as Heroku.MIME
import           Fission.Web.Server
import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.UserConfig as Heroku
import           Fission.Platform.Heroku.Provision  as Provision

import qualified Fission.Config as Config
import qualified Fission.Random as Random

import           Fission.Storage.Query

import           Fission.User       as User
import qualified Fission.User.Table as Table

import           Fission.Platform.Heroku.AddOn       as AddOn
import qualified Fission.Platform.Heroku.AddOn.Table as Table

import           Fission.Security.Types (Secret (..))

type API = CreateAPI :<|> DeleteAPI

type CreateAPI = ReqBody '[JSON]                     Provision.Request
              :> Post    '[Heroku.MIME.VendorJSONv3] Provision

server :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
server = undefined

create :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg CreateAPI
create Request {_uuid, _region} = do
  Web.Host url <- Config.get
  secret       <- liftIO $ Random.text 200
  userID       <- User.create _uuid _region secret

  logInfo $ mconcat
    [ "Provisioned UUID: "
    , displayShow _uuid
    , " as "
    , displayShow userID
    ]

  let
    userConfig = Heroku.UserConfig
      { Heroku._interplanetaryFissionUrl      = url
      , Heroku._interplanetaryFissionUsername = User.hashID userID
      , Heroku._interplanetaryFissionPassword = Secret secret
      }

  return Provision
    { _id      = userID
    , _config  = userConfig
    , _message = "Successfully provisioned Interplanetary FISSION!"
    }

type DeleteAPI = Capture "addon_id" UUID
              :> DeleteNoContent '[] NoContent

delete :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg DeleteAPI
delete uuid =
  Query.oneEq Table.addOns uuid' uuid >>= \case
    Nothing ->
      throwM err404

    Just AddOn {_addOnID} -> do
      transaction do
        deleteFrom_ Table.addOns (uuid' `is` uuid)

        Query.oneEq Table.users herokuAddOnId' (Just _addOnID) >>= \case
          Nothing -> do
            logError "Unable to find a user for Heroku AddOn " <> displayShow uuid
            return ()

          Just User {_userID} ->
            User.deactivate _userID

      return NoContent
