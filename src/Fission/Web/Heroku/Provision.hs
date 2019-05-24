{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BlockArguments    #-}

module Fission.Web.Heroku.Provision
  ( API
  , server
  ) where

import RIO
import RIO.Text

import Data.Has

import Servant.API

import Data.Pool
import Data.Time

import Database.Selda

import Fission.Platform.Heroku.Provision  as Provision
import Fission.Platform.Heroku.UserConfig

import qualified Fission.Platform.Heroku.Host as Host
import qualified Fission.Web.Heroku.MIME      as Heroku
import           Fission.Web.Server
import           Fission.Config
import           Fission.User
import           Fission.Storage.SQLite


type API = ReqBody '[JSON]                Provision.Request
        :> Post    '[Heroku.VendorJSONv3] Provision

server :: HasLogFunc cfg
       => Has DBPool cfg
       => MonadSelda IO
       => RIOServer cfg API
server Request {_uuid} = do
  logInfo $ "Provisioning UUID: " <> displayShow _uuid
  DBPool pool <- view hasLens
  now         <- liftIO getCurrentTime
  userId      <- liftIO . withResource pool . const . insert1 now $
                  User def Regular Nothing

  return Provision
    { _id      = userId
    , _config  = UserConfig $ pack Host.api
    , _message = "Provisioned successfully"
    }
