{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

module Fission.Web.Heroku.Schema (API, run) where

import RIO

import Servant.API
import Servant.Client (client)

import qualified Fission.Platform.Heroku.Host as Heroku.Host
import qualified Fission.Web.Client           as Client
import qualified Fission.Web.Heroku.MIME      as Heroku

type API = "schema" :> Get '[Heroku.VendorJSONv3] Text

run :: HasLogFunc cfg => RIO cfg ()
run = Client.run Heroku.Host.api $ client api

api :: Proxy API
api = Proxy
