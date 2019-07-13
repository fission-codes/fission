{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.Heroku
  ( API
  , create
  ) where

import RIO

import Data.Has
import Database.Selda
import Servant

import qualified Fission.Web.Heroku.MIME as Heroku.MIME
import           Fission.Web.Server
import qualified Fission.Web.Types as Web

import qualified Fission.Platform.Heroku.UserConfig as Heroku
import           Fission.Platform.Heroku.Provision  as Provision

import qualified Fission.Config as Config
import qualified Fission.Random as Random
import qualified Fission.User   as User

import Fission.Security.Types (Secret (..))

type API = "resources" :> CreateAPI

type CreateAPI = ReqBody '[JSON]                     Provision.Request
              :> Post    '[Heroku.MIME.VendorJSONv3] Provision

create :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
create Request {_uuid, _region} =
  transaction do
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
        { Heroku._interplanetaryFissionUrl      = url <> "/ipfs"
        , Heroku._interplanetaryFissionUsername = User.hashID userID
        , Heroku._interplanetaryFissionPassword = Secret secret
        }

    return Provision
      { _id      = userID
      , _config  = userConfig
      , _message = "Successfully provisioned Interplanetary FISSION!"
      }
