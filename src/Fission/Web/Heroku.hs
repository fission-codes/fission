{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.Heroku
  ( API
  , create
  ) where

import RIO
import RIO.Text

import Database.Selda
import Servant.API
import Servant

import           Fission.Platform.Heroku.Provision  as Provision
import qualified Fission.Platform.Heroku.UserConfig as Heroku
import qualified Fission.Platform.Heroku.Host       as Host

import qualified Fission.Web.Heroku.MIME      as Heroku
import           Fission.Web.Server

import qualified Fission.User as User
import           Fission.Random as Random
import qualified Fission.Internal.UTF8 as UTF8

type API = "resources" :> CreateAPI

type CreateAPI = ReqBody '[JSON]                Provision.Request
              :> Post    '[Heroku.VendorJSONv3] Provision

create :: HasLogFunc cfg => MonadSelda (RIO cfg) => RIOServer cfg API
create (Request {_uuid, _region}) = do
  sekret <- liftIO $ Random.text 500 >>= \case
    Left  err -> throwM $ err500 { errBody = UTF8.showLazyBS err }
    Right txt -> pure txt

  userId <- User.createFresh _uuid _region sekret
  logInfo $ "Provisioned UUID: " <> displayShow _uuid <> " as " <> displayShow userId

  return Provision
    { _id      = userId
    , _config  = Heroku.UserConfig (pack Host.api) sekret
    , _message = "Provisioned successfully"
    }
