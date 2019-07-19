module Fission.Web.Routes
  ( API
  , HerokuRoute
  , IPFSRoute
  , PingRoute
  , PublicAPI
  ) where

import RIO

import Servant

import           Fission.User
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Heroku as Heroku

type API = IPFSRoute :<|> HerokuRoute :<|> PingRoute

type PublicAPI = IPFSRoute

type IPFSRoute = "ipfs"
                 :> BasicAuth "registered users" User
                 :> IPFS.API

type HerokuRoute = "heroku"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type PingRoute = "ping" :> Ping.API
