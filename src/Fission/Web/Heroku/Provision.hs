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

import Data.Time
import Database.Selda
import Servant.API

import           Fission.Platform.Heroku.Provision  as Provision
import           Fission.Platform.Heroku.UserConfig
import qualified Fission.Platform.Heroku.Host       as Host

import qualified Fission.Web.Heroku.MIME      as Heroku
import           Fission.Web.Server

import           Fission.User
import           Fission.Storage.SQLite

type API = ReqBody '[JSON]                Provision.Request
        :> Post    '[Heroku.VendorJSONv3] Provision

server :: HasLogFunc cfg
       => MonadSelda (RIO cfg)
       => RIOServer cfg API
server Request {_uuid} = do
  now    <- liftIO getCurrentTime
  userId <- insert1 now $ User def Regular Nothing

  logInfo $ "Provisioned UUID: " <> displayShow _uuid <> " as " <> displayShow userId

  return Provision
    { _id      = userId
    , _config  = UserConfig $ pack Host.api
    , _message = "Provisioned successfully"
    }
