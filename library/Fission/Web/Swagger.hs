module Fission.Web.Swagger
  ( API
  , server
  ) where

import           Data.Swagger
import           Servant
import           Servant.Client (BaseUrl (..))
import           Servant.Swagger
import           Servant.Swagger.UI
import qualified Servant.Swagger.Internal.TypeLevel.API as Servant.API

import           Fission.Prelude
import           Fission.Internal.Orphanage.BasicAuth     ()
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.MultipartForm ()

import qualified Fission.Web.Routes as Web
import           Fission.Web.Server
import qualified Fission.Web.Types as Web

type API = SwaggerSchemaUI "docs" "docs.json"

server :: Web.Host -> RIOServer cfg API
server appHost =
  appHost
    |> docs
    |> swaggerSchemaUIServer
    |> hoistServer (Proxy @API) fromHandler

docs :: Web.Host -> Swagger
docs host' =
  host'
    |> app (Proxy @Web.API)
    |> dns
    |> ipfs
    |> heroku
    |> ping
    |> user

app :: HasSwagger api => Proxy api -> Web.Host -> Swagger
app proxy (Web.Host (BaseUrl { baseUrlHost })) =
  proxy
    |> toSwagger
    |> host               ?~ Host baseUrlHost Nothing
    |> schemes            ?~ [Https]
    |> info . title       .~ "The Fission API"
    |> info . version     .~ "2.0.0"
    |> info . description ?~ blurb
    |> info . contact     ?~ fissionContact
    |> info . license     ?~ projectLicense
  where
    fissionContact =
      mempty
        |> name  ?~"Team Fission"
        |> url   ?~ URL "https://fission.codes"
        |> email ?~ "support@fission.codes"

    projectLicense =
      "Apache 2.0" |> url ?~ URL "http://www.apache.org/licenses/LICENSE-2.0"

    blurb =
      "Bootstrapped & distributed backend-as-a-service with user-controlled data"

dns :: Swagger -> Swagger
dns = makeDocs (Proxy @Web.DNSRoute)
  ["DNS" |> description ?~ "DNS management"]

user :: Swagger -> Swagger
user = makeDocs (Proxy @Web.UserRoute)
  ["Users" |> description ?~ "Accounts, authentication, and stats"]

heroku :: Swagger -> Swagger
heroku = makeDocs (Proxy @Web.HerokuRoute)
  ["Heroku" |> description ?~ "Interaction with the Heroku add-on API"]

ipfs :: Swagger -> Swagger
ipfs = makeDocs (Proxy @Web.IPFSRoute)
  ["IPFS" |> description ?~ "The primary IPFS API"]

ping :: Swagger -> Swagger
ping = makeDocs (Proxy @Web.PingRoute)
  ["Ping" |> description ?~ "Check for liveness"]

makeDocs
  :: ( Servant.API.IsSubAPI subRoute Web.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> [Tag]
  -> Swagger
  -> Swagger
makeDocs routeProxy = applyTagsFor (subOps routeProxy)

subOps
  :: ( Applicative f
     , Servant.API.IsSubAPI subRoute Web.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> (Operation -> f Operation)
  ->   Swagger
  -> f Swagger
subOps routeProxy = subOperations routeProxy <| Proxy @Web.API
