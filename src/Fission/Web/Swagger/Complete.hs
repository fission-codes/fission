module Fission.Web.Swagger.Complete
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

type API = SwaggerSchemaUI "all_docs" "all_docs.json"

server :: Host -> RIOServer cfg API
server appHost =
  hoistServer (Proxy :: Proxy API) fromHandler . swaggerSchemaUIServer $ docs appHost

docs :: Host -> Swagger
docs = Docs.ipfs
     . Docs.heroku
     . Docs.ping
     . Docs.app (Proxy :: Proxy Web.API)
