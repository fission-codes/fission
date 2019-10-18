-- | Top-level router type for the application
module Fission.Web.Routes
  ( API
  , UserRoute
  , HerokuRoute
  , IPFSPrefix
  , IPFSRoute
  , PingRoute
  , DNSRoute
  ) where

import Servant

import qualified Fission.Web.IPFS       as IPFS
import qualified Fission.Web.Ping       as Ping
import qualified Fission.Web.Heroku     as Heroku
import qualified Fission.Web.DNS        as DNS
import qualified Fission.Web.Auth.Types as Auth
import qualified Fission.Web.User       as User

import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Auth.Verify as Auth.Verify
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.Heroku      as Heroku

import           Fission.User.Types
import qualified Fission.Web.IPFS        as IPFS
import qualified Fission.Web.Auth.Verify as Auth.Verify
import qualified Fission.Web.Ping        as Ping
import qualified Fission.Web.Heroku      as Heroku
import qualified Fission.Web.DNS         as DNS
import qualified Fission.Web.Auth        as Auth

import           Fission.User.Types
import qualified Fission.Web.User        as User
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Heroku as Heroku
import qualified Fission.Web.User   as User
import qualified Fission.Web.DNS    as DNS
import qualified Fission.Web.Auth   as Auth

type API = IPFSRoute
      :<|> HerokuRoute
      :<|> UserRoute
      :<|> PingRoute
      :<|> DNSRoute

type UserRoute = "user" :> User.API
type PingRoute = "ping" :> Ping.API

type IPFSRoute  = IPFSPrefix :> IPFS.API
type IPFSPrefix = "ipfs"

type HerokuRoute = "heroku"
                   :> "resources"
                   :> Auth.HerokuAddOnAPI
                   :> Heroku.API

type DNSRoute = "dns"
                :> Auth.ExistingUser
                :> DNS.API
