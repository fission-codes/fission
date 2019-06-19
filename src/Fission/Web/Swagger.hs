module Fission.Web.Swagger
  ( API
  , docs
  ) where

import RIO

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Fission.Web.Server

import qualified Fission.Web.Routes as Web

type API = SwaggerSchemaUI "swagger-ui" "swagger.json"

server :: RIOServer (SwaggerSchemaUI' dir API)
server = pure $ swaggerSchemaUIServer' docs

docs :: Swagger
docs = toSwagger (Proxy :: Proxy Web.API)
