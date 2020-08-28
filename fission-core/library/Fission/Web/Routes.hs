-- | Top-level router type for the application
module Fission.Web.Routes
  ( API
  , UserPrefix
  , UserRoute
  , HerokuRoute
  , IPFSPrefix
  , IPFSRoute
  , PingRoute
  , DNSRoute
  , AppRoute
  , AppPrefix
  ) where

import           Servant

import qualified Fission.Web.IPFS       as IPFS
import qualified Fission.Web.Ping       as Ping
import qualified Fission.Web.Heroku     as Heroku
import qualified Fission.Web.DNS        as DNS
import qualified Fission.Web.Auth.Types as Auth
import qualified Fission.Web.User       as User
import qualified Fission.Web.App        as App

type API
  =    IPFSRoute
  :<|> AppRoute
  :<|> HerokuRoute
  :<|> UserRoute
  :<|> PingRoute
  :<|> DNSRoute

type AppRoute
  =  AppPrefix
  :> Auth.HigherOrder
  :> App.API

type AppPrefix
  = "app"

type PingRoute
  =  "ping"
  :> Ping.API

type UserRoute
  =  UserPrefix
  :> User.API

type UserPrefix
  = "user"

type IPFSRoute
  =  IPFSPrefix
  :> IPFS.API

type IPFSPrefix
  = "ipfs"

type HerokuRoute
  =  "heroku"
  :> "resources"
  :> Auth.HerokuAddOnAPI
  :> Heroku.API

type DNSRoute
  =  "dns"
  :> Auth.HigherOrder
  :> DNS.API
