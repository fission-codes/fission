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
  , create
  ) where

import RIO

import Data.Has
import Database.Selda
import Servant.API

import           Fission.Config
import qualified Fission.Platform.Heroku           as Heroku
import           Fission.Platform.Heroku.Provision as Provision

import qualified Fission.Web.Heroku.MIME as Heroku
import           Fission.Web.Server

import           Fission.Security as Security
import qualified Fission.User     as User
import qualified Fission.Random   as Random

type API = ReqBody '[JSON]                Provision.Request
        :> Post    '[Heroku.VendorJSONv3] Provision

create :: (HasLogFunc cfg, Has Host cfg, MonadSelda (RIO cfg)) => RIOServer cfg API
create (Request {_uuid, _region}) = do
  Host url <- view hasLens
  secret   <- liftIO $ Random.text 250
  userId   <- User.createFresh _uuid _region secret

  logInfo $ mconcat
    [ "Provisioned UUID: "
    , displayShow _uuid
    , " as "
    , displayShow userId
    ]

  return Provision
    { _id      = userId
    , _config  = Heroku.UserConfig url (digest userId) (Secret secret)
    , _message = "Successfully provisioned Interplanetary FISSION!"
    }
