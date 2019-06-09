{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku
  ( API
  , create
  ) where

import RIO

import Data.Has
import Database.Selda
import Servant

import qualified Fission.Web.Heroku.MIME as Heroku
import           Fission.Web.Server

import qualified Fission.Platform.Heroku           as Heroku
import           Fission.Platform.Heroku.Provision as Provision

import qualified Fission.Random                    as Random
import           Fission.Security
import qualified Fission.User                      as User
import           Fission.Config

--------------------------------------------------------------------------------

type API = "resources" :> CreateAPI

------------
-- CREATE --
------------

type CreateAPI = ReqBody '[JSON]                Provision.Request
              :> Post    '[Heroku.VendorJSONv3] Provision

create :: (HasLogFunc cfg, Has Host cfg, MonadSelda (RIO cfg)) => RIOServer cfg API
create (Request {_uuid, _region}) = do
  Host url <- fromCfg
  secret   <- liftIO $ Random.text 200
  userID   <- User.createFresh _uuid _region secret

  logInfo $ mconcat
    [ "Provisioned UUID: "
    , displayShow _uuid
    , " as "
    , displayShow userID
    ]

  let
    userConfig = Heroku.UserConfig
      { Heroku._fissionApiUrl   = url
      , Heroku._fissionUserName = digest userID
      , Heroku._fissionSecret   = Secret secret
      }

  return Provision
    { _id      = userID
    , _config  = userConfig
    , _message = "Successfully provisioned Interplanetary FISSION!"
    }
