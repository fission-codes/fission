{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku where

import RIO

import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client

import Fission.Web.Heroku.MIME as Heroku
import Fission.Web.Tls         as Tls

-- type ProvisionAPI = ReqBody '[JSON] Provision.Request
--                  :> Post    '[JSON] Provision

type API = "schema" :> Get '[Heroku.VendorJSONv3] Heroku.VendorJSONv3
