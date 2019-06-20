module Fission.Web.Swagger
  ( API
  , docs
  , server
  ) where

import RIO

import Data.Swagger
import Servant
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI.ReDoc

import qualified Fission.Web.Routes as Web
import Fission.Web.Server

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"

server :: RIOServer cfg API
server = hoistServer (Proxy :: Proxy API) fromHandler (redocSchemaUIServer docs)

docs :: Swagger
docs = toSwagger (Proxy :: Proxy Web.API)
