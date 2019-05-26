{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku.Resource
  ( API
  -- , create
  ) where

import RIO
import RIO.Text

import Database.Selda
import Servant.API

import qualified Fission.Platform.Heroku.Host       as Host
import           Fission.Platform.Heroku.Provision  as Provision
import qualified Fission.Platform.Heroku.UserConfig as Heroku

import qualified Fission.Web.Heroku.MIME as Heroku
import           Fission.Web.Server

import qualified Fission.User as User

type API = ReqBody '[JSON]                Provision.Request
        :> Post    '[Heroku.VendorJSONv3] Provision

-- create :: HasLogFunc cfg
--        => MonadSelda (RIO cfg)
--        => RIOServer cfg API
-- create (Request {_uuid, _region}) = do
--   userId <- User.createFresh _uuid _region
--   logInfo $ "Provisioned UUID: " <> displayShow _uuid <> " as " <> displayShow userId

--   return Provision
--     { _id      = userId
--     , _config  = Heroku.UserConfig $ pack Host.api
--     , _message = "Provisioned successfully"
--     }
