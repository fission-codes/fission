module Fission.Web.Routes
  ( API
  , UserRoute
  , HerokuRoute
  , IPFSPrefix
  , IPFSRoute
  , PingRoute
  ) where

import RIO

import Servant

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Auth.Verify as Auth.Verify
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.Heroku      as Heroku
import qualified Fission.Web.Domain      as Domain

import           Fission.User.Types

type API = IPFSRoute
      :<|> HerokuRoute
      :<|> UserRoute
      :<|> PingRoute
      :<|> DomainRoute

type UserRoute = "user" :> User.API
type PingRoute = "ping" :> Ping.API

type IPFSRoute  = IPFSPrefix :> IPFS.API
type IPFSPrefix = "ipfs"

type HerokuRoute = "heroku"
                   :> "resources"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type DomainRoute = "domain"
                  :> BasicAuth "existing user" User
                  :> Domain.API
