module Fission.Web.Swagger.Public
  ( API
  , docs
  , server
  ) where

import RIO

import Data.Swagger

import Servant
import Servant.Swagger.UI

import qualified Fission.Web.Routes       as Web
import qualified Fission.Web.Swagger.Docs as Docs
import           Fission.Web.Server

type API = SwaggerSchemaUI "docs" "docs.json"

server :: Host -> RIOServer cfg API
server appHost =
  hoistServer (Proxy :: Proxy API) fromHandler . swaggerSchemaUIServer $ docs appHost

docs :: Host -> Swagger
docs = Docs.ipfs . Docs.app (Proxy :: Proxy Web.PublicAPI)
