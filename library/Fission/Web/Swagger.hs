module Fission.Web.Swagger
  ( API
  , server
  ) where

import           Data.Swagger

import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI.ReDoc
import qualified Servant.Swagger.Internal.TypeLevel.API as Servant.API

import           Fission.Prelude

import           Fission.Internal.Orphanage.BasicAuth     ()
import           Fission.Internal.Orphanage.BasicAuthData ()
import           Fission.Internal.Orphanage.HigherOrder   ()
import           Fission.Internal.Orphanage.RegisterDid   ()

import qualified Fission.Internal.Meta as Meta

import qualified Fission.Web.Routes as Web
import qualified Fission.Web.Types  as Web

type API = SwaggerSchemaUI "docs" "docs.json"

server :: (forall a . Handler a -> m a) -> Web.Host -> ServerT API m
server fromHandler appHost =
  appHost
    |> docs
    |> redocSchemaUIServer
    |> hoistServer (Proxy @API) fromHandler

docs :: Web.Host -> Swagger
docs host' =
  host'
    |> fission (Proxy @Web.API)
    |> app
    |> dns
    |> heroku
    |> ipfs
    |> ping
    |> user

fission :: HasSwagger api => Proxy api -> Web.Host -> Swagger
fission proxy appHost =
  proxy
    |> toSwagger
    |> host               ?~ Host (Web.getRawHost appHost) Nothing
    |> schemes            ?~ [Https]
    |> info . title       .~ "The Fission API"
    |> info . version     .~ version'
    |> info . description ?~ blurb
    |> info . contact     ?~ fissionContact
    |> info . license     ?~ projectLicense
  where
    version' =
      Meta.package
        |> bind Meta.version
        |> maybe "unknown" identity

    fissionContact =
      mempty
        |> name  ?~"Team Fission"
        |> url   ?~ URL "https://fission.codes"
        |> email ?~ "support@fission.codes"

    projectLicense =
      "AGPL 3.0" |> url ?~ URL "https://www.gnu.org/licenses/agpl-3.0.en.html"

    blurb =
      "Bootstrapped & distributed backend-as-a-service with user-controlled data"

app :: Swagger -> Swagger
app = makeDocs (Proxy @Web.AppRoute)
  ["App" |> description ?~ "Hosted applications"]

dns :: Swagger -> Swagger
dns = makeDocs (Proxy @Web.DNSRoute)
  ["DNS" |> description ?~ "DNS management"]

user :: Swagger -> Swagger
user = makeDocs (Proxy @Web.UserRoute)
  ["User" |> description ?~ "Accounts, authentication, and stats"]

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
subOps routeProxy = subOperations routeProxy $ Proxy @Web.API
