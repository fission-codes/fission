module Fission.Web.Routes
  ( API
  , HerokuRoute
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

import           Fission.User.Types

type API = IPFSRoute
      :<|> HerokuRoute
      :<|> AuthRoute
      :<|> PingRoute

type PublicAPI = IPFSRoute

type IPFSRoute = "ipfs" :> IPFS.API

type HerokuRoute = "heroku"
                   :> "resources"
                   :> BasicAuth "heroku add-on api" ByteString
                   :> Heroku.API

type AuthRoute = "auth"
               :> "verify"
               :> BasicAuth "existing user" User
               :> Auth.Verify.API

type PingRoute = "ping" :> Ping.API
