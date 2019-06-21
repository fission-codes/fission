module Fission.Web.Swagger
  ( API
  , docs
  , server
  ) where

import RIO

import Control.Lens
import Data.Swagger

import Servant
import Servant.Swagger
import Servant.Swagger.UI

import qualified Fission.Web.Routes as Web
import           Fission.Web.Server

type API = SwaggerSchemaUI "docs" "docs.json"

server :: Host -> RIOServer cfg API
server appHost =
  hoistServer (Proxy :: Proxy API) fromHandler . swaggerSchemaUIServer $ docs appHost

docs :: Host -> Swagger
docs appHost = toSwagger (Proxy :: Proxy Web.API)
             & host               ?~ appHost
             & schemes            ?~ [Https, Http]
             & info . title       .~ "FISSION's IPFS API"
             & info . version     .~ "1.0.0"
             & info . description ?~ "Easily use IPFS from Web 2.0 applications"
             & info . contact     ?~ fissionContact
             & info . license     ?~ projectLicense
             & ipfsDocs
             & herokuDocs
             & pingDocs
  where
    fissionContact = mempty
                   & name ?~"FISSION Team"
                   & url ?~ URL "https://fission.codes"
                   & email ?~ "support@fission.codes"

    projectLicense = "Apache 2.0"
                   & url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0"

ipfsDocs :: Swagger -> Swagger
ipfsDocs = applyTagsFor ops ["IPFS" & description ?~ "The primary IPFS API"]
  where
    ops = subOperations (Proxy :: Proxy Web.IPFSRoute) (Proxy :: Proxy Web.API)

herokuDocs :: Swagger -> Swagger
herokuDocs = applyTagsFor ops  ["Heroku" & description ?~ "Interaction with the Heroku add-on API"]
  where
    ops = subOperations (Proxy :: Proxy Web.HerokuRoute) (Proxy :: Proxy Web.API)

pingDocs :: Swagger -> Swagger
pingDocs = applyTagsFor ops  ["Ping" & description ?~ "Check for liveness"]
  where
    ops = subOperations (Proxy :: Proxy Web.PingRoute) (Proxy :: Proxy Web.API)
