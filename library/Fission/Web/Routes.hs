module Fission.Web.Routes
  ( API
  , AuthRoute
  , HerokuRoute
  , IPFSPrefix
  , IPFSRoute
  , PingRoute
  , PublicAPI
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
      :<|> AuthRoute
      :<|> PingRoute
      :<|> DomainRoute

type PublicAPI = IPFSRoute
            :<|> AuthRoute

type AuthRoute = "auth"
               :> "verify"
               :> BasicAuth "existing user" User
               :> Auth.Verify.API

type HerokuRoute = "heroku"
                   :> "resources"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type IPFSRoute  = IPFSPrefix :> IPFS.API
type IPFSPrefix = "ipfs"

type PingRoute = "ping" :> Ping.API

type DomainRoute = "domain" 
                  :> BasicAuth "existing user" User
                  :> Domain.API
