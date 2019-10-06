module Fission.Web.Swagger
  ( API
  , server
  ) where

import RIO

import Control.Lens
import Data.Swagger

import Servant
import Servant.Swagger
import Servant.Swagger.UI

import           Fission.Internal.Orphanage.BasicAuth     ()
import           Fission.Internal.Orphanage.MultipartForm ()

import qualified Fission.Web.Routes       as Web
import           Fission.Web.Server

type API = SwaggerSchemaUI "docs" "docs.json"

server :: Host -> RIOServer cfg API
server = hoistServer (Proxy :: Proxy API) fromHandler . swaggerSchemaUIServer . docs

docs :: Host -> Swagger
docs = ipfs
     . heroku
     . ping
     . app (Proxy :: Proxy Web.API)

app :: HasSwagger api => Proxy api -> Host -> Swagger
app proxy appHost = toSwagger proxy
                  & host               ?~ appHost
                  & schemes            ?~ [Https, Http]
                  & info . title       .~ "FISSION's IPFS API"
                  & info . version     .~ "1.0.0"
                  & info . description ?~ "Easily use IPFS from Web 2.0 applications"
                  & info . contact     ?~ fissionContact
                  & info . license     ?~ projectLicense
  where
    fissionContact = mempty
                   & name  ?~"FISSION Team"
                   & url   ?~ URL "https://fission.codes"
                   & email ?~ "support@fission.codes"

    projectLicense = "Apache 2.0"
                   & url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0"

auth :: Swagger -> Swagger
auth = applyTagsFor ops  ["Authentication" & description ?~ "Auth actions & verification"]
  where
    ops = subOperations (Proxy :: Proxy Web.AuthRoute) (Proxy :: Proxy Web.API)

heroku :: Swagger -> Swagger
heroku = applyTagsFor ops  ["Heroku" & description ?~ "Interaction with the Heroku add-on API"]
  where
    ops = subOperations (Proxy :: Proxy Web.HerokuRoute) (Proxy :: Proxy Web.API)

ipfs :: Swagger -> Swagger
ipfs = applyTagsFor ops ["IPFS" & description ?~ "The primary IPFS API"]
  where
    ops = subOperations (Proxy :: Proxy Web.IPFSRoute) (Proxy :: Proxy Web.API)

ping :: Swagger -> Swagger
ping = applyTagsFor ops  ["Ping" & description ?~ "Check for liveness"]
  where
    ops = subOperations (Proxy :: Proxy Web.PingRoute) (Proxy :: Proxy Web.API)
