module Fission.Web.Routes
  ( API
  , UserPrefix
  , UserRoute
  , HerokuRoute
  , IPFSPrefix
  , IPFSRoute
  , PingRoute
  , PublicAPI
  ) where

import RIO

import Servant

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.Heroku      as Heroku
import qualified Fission.Web.User        as User

type API = IPFSRoute
      :<|> HerokuRoute
      :<|> UserRoute
      :<|> PingRoute

type PublicAPI = IPFSRoute
            :<|> UserRoute

type UserRoute = UserPrefix :> User.API
type UserPrefix = "user"

type HerokuRoute = "heroku"
                   :> "resources"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type IPFSRoute  = IPFSPrefix :> IPFS.API
type IPFSPrefix = "ipfs"

type PingRoute = "ping" :> Ping.API
