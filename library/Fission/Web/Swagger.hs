module Fission.Web.Swagger
  ( API
  , server
  ) where

import RIO

import Control.Lens
import Data.Swagger

import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import qualified Servant.Swagger.Internal.TypeLevel.API as Servant.API

import           Fission.Internal.Orphanage.BasicAuth     ()
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.MultipartForm ()

import qualified Fission.Web.Routes as Web
import           Fission.Web.Server

type API = SwaggerSchemaUI "docs" "docs.json"

server :: Host -> RIOServer cfg API
server = hoistServer (Proxy @API) fromHandler . swaggerSchemaUIServer . docs

docs :: Host -> Swagger
docs host' = dns
           . ipfs
           . heroku
           . ping
           . user
           $ app (Proxy @Web.API) host'

app :: HasSwagger api => Proxy api -> Host -> Swagger
app proxy appHost = toSwagger proxy
                  & host               ?~ appHost
                  & schemes            ?~ [Https, Http]
                  & info . title       .~ "The Fission API"
                  & info . version     .~ "1.12.0"
                  & info . description ?~ blurb
                  & info . contact     ?~ fissionContact
                  & info . license     ?~ projectLicense
  where
    fissionContact = mempty
                   & name  ?~"Team Fission"
                   & url   ?~ URL "https://fission.codes"
                   & email ?~ "support@fission.codes"

    projectLicense = "Apache 2.0"
                   & url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0"

    blurb = "Bootstrapped & distributed backend-as-a-service with user-controlled data"

user :: Swagger -> Swagger
user = makeDocs (Proxy @Web.UserRoute)
  ["Users" & description ?~ "Accounts, authentication, and stats"]

heroku :: Swagger -> Swagger
heroku = makeDocs (Proxy @Web.HerokuRoute)
  ["Heroku" & description ?~ "Interaction with the Heroku add-on API"]

ipfs :: Swagger -> Swagger
ipfs = makeDocs (Proxy @Web.IPFSRoute)
  ["IPFS" & description ?~ "The primary IPFS API"]

ping :: Swagger -> Swagger
ping = makeDocs (Proxy @Web.PingRoute)
  ["Ping" & description ?~ "Check for liveness"]

dns :: Swagger -> Swagger
dns = makeDocs (Proxy @Web.DNSRoute)
  ["DNS" & description ?~ "Interact with DNS on AWS Route53"]

makeDocs :: Servant.API.IsSubAPI subRoute Web.API
         => HasSwagger subRoute
         => Proxy subRoute
         -> [Tag]
         -> Swagger
         -> Swagger
makeDocs routeProxy = applyTagsFor (subOps routeProxy)

subOps :: Applicative f
       => Servant.API.IsSubAPI subRoute Web.API
       => HasSwagger subRoute
       => Proxy subRoute
       -> (Operation -> f Operation)
       ->   Swagger
       -> f Swagger
subOps routeProxy = subOperations routeProxy $ Proxy @Web.API
