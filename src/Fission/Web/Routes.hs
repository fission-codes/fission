module Fission.Web.Routes (API) where

import RIO

import Servant

import           Fission.User
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Heroku as Heroku

type API = "ipfs"
           :> BasicAuth "registered users" User
           :> IPFS.API
      :<|> "heroku"
           :> BasicAuth "heroku add-on api" ByteString
           :> Heroku.API
      :<|> "ping"
           :> Ping.API
