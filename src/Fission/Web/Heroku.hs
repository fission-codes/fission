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
import RIO.Text

import Crypto.Hash
import Database.Selda
import Servant

import qualified Fission.Platform.Heroku.Host       as Host
import           Fission.Platform.Heroku.Provision  as Provision
import           Fission.Platform.Heroku.UserConfig as Heroku

import qualified Fission.Web.Heroku.MIME as Heroku
import           Fission.Web.Server

import qualified Fission.Internal.UTF8 as UTF8
import           Fission.Random        as Random
import           Fission.Security      as Security
import qualified Fission.User          as User

type API = "resources" :> CreateAPI

type CreateAPI = ReqBody '[JSON]                Provision.Request
              :> Post    '[Heroku.VendorJSONv3] Provision

create :: (HasLogFunc cfg, MonadSelda (RIO cfg)) => RIOServer cfg API
create (Request {_uuid, _region}) = do
  rawSecret <- liftIO $ Random.text 500
  secret'   <- case Security.toSecret rawSecret of
    Left  unicodeErr -> throwM $ err500 { errBody = UTF8.showLazyBS unicodeErr }
    Right skt        -> return skt

  userId <- User.createFresh _uuid _region
    (UTF8.textShow (hash rawSecret :: Digest SHA3_512))

  logInfo $ mconcat
    [ "Provisioned UUID:"
    , displayShow _uuid
    , " as "
    , displayShow userId
    ]

  return Provision
    { _id      = userId
    , _message = "Successfully provisioned Fission for Heroku"
    , _config  = Heroku.UserConfig
                   { _fissionApiUrl = pack Host.api
                   , _fissionSecret = secret'
                   }
    }
