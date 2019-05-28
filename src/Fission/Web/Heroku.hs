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

import Database.Selda
import Servant

-- Heroku
import qualified Fission.Platform.Heroku.Host       as Host
import           Fission.Platform.Heroku.Provision  as Provision
import           Fission.Platform.Heroku.UserConfig as Heroku

-- Web
import           Fission.Web.Error
import qualified Fission.Web.Heroku.MIME as Heroku
import           Fission.Web.Server

-- Utility
import qualified Fission.Random   as Random
import           Fission.Security as Security
import qualified Fission.User     as User

--------------------------------------------------------------------------------

type API = "resources" :> CreateAPI

------------
-- CREATE --
------------

type CreateAPI = ReqBody '[JSON]                Provision.Request
              :> Post    '[Heroku.VendorJSONv3] Provision

create :: (HasLogFunc cfg, MonadSelda (RIO cfg)) => RIOServer cfg API
create (Request {_uuid, _region}) = do
  rawSecret <- liftIO $ Random.byteString 500
  secret'   <- ensureUnicode $ Security.toSecret rawSecret
  userId    <- User.createFresh _uuid _region $ toHash rawSecret

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
                   { _fissionApiUrl   = pack Host.api
                   , _fissionUserName = userId
                   , _fissionSecret   = secret'
                   }
    }
