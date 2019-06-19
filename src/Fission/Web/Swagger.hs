module Fission.Web.Swagger where

import RIO

import Data.Swagger
import Servant
import Servant.Auth.Swagger as Swagger
import Servant.Swagger

import qualified Fission.Web.Auth   as Auth
import qualified Fission.Web.IPFS   as IPFS
import qualified Fission.Web.Ping   as Ping
import qualified Fission.Web.Types  as Web

import qualified Fission.Platform.Heroku.Types as Heroku
import qualified Fission.Web.Heroku            as Heroku

type API = Get '[JSON] Swagger

type DocAPI =
  "ipfs"
  --               -- :> Servant.Auth.Swagger.BasicAuth
                :> IPFS.API
          :<|>
          "heroku"
                -- :> Swagger.BasicAuth
                :> Heroku.API
          :<|> "ping"
                :> Ping.API

docs :: Swagger
docs = toSwagger api

api :: Proxy DocAPI
api = Proxy
