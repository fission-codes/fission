{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku.Provision
  ( API
  , server
  ) where

import RIO
import RIO.Text

import Servant.API

import Fission.Partner.Heroku.Provision  as Provision
import Fission.Partner.Heroku.UserConfig

import qualified Fission.Partner.Heroku.Host as Host
import qualified Fission.Web.Heroku.MIME     as Heroku
import           Fission.Web.Server

type API = ReqBody '[JSON]                Provision.Request
        :> Post    '[Heroku.VendorJSONv3] Provision

server :: HasLogFunc cfg => RIOServer cfg API
server Request {_uuid} = do
  logInfo $ "Provisioning UUID: " <> displayShow _uuid

  return $ Provision
    { _id      = _uuid -- PROBABLY WRONG
    , _config  = UserConfig $ pack Host.api
    , _message = "Provisioned successfully"
    }
