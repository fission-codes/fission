module Fission.Web.Server.Swagger (handler) where

import           Data.Swagger
import qualified Data.Version                                        as Version

import qualified RIO.Text                                            as Text

import           Servant
import           Servant.Swagger
import qualified Servant.Swagger.Internal.TypeLevel.API              as Servant.API
import           Servant.Swagger.UI.ReDoc

import           Fission.Prelude

import qualified Fission.Web.API.App.Types                           as API
import qualified Fission.Web.API.DNS.Types                           as API
import qualified Fission.Web.API.Heroku.Types                        as API
import qualified Fission.Web.API.IPFS.Types                          as API
import qualified Fission.Web.API.Ping.Types                          as API
import qualified Fission.Web.API.User.Types                          as API

import qualified Fission.Web.API.Types                               as Fission

import qualified Fission.Web.Server.Host.Types                       as Web
import qualified Fission.Web.Server.Swagger.Types                    as Swagger

import           Fission.Web.Server.Internal.Orphanage.BasicAuth     ()
import           Fission.Web.Server.Internal.Orphanage.BasicAuthData ()
import           Fission.Web.Server.Internal.Orphanage.HigherOrder   ()
import           Fission.Web.Server.Internal.Orphanage.RegisterDid   ()

import qualified Paths_fission_web_server                            as Fission

handler :: (forall a . Handler a -> m a) -> Web.Host -> ServerT Swagger.API m
handler fromHandler appHost =
  appHost
    |> docs
    |> redocSchemaUIServer
    |> hoistServer (Proxy @Swagger.API) fromHandler

docs :: Web.Host -> Swagger
docs host' =
  host'
    |> fission (Proxy @Fission.API)
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
      Text.pack $ showVersion Fission.version

    fissionContact =
      mempty
        |> name  ?~ "Team Fission"
        |> url   ?~ URL "https://fission.codes"
        |> email ?~ "support@fission.codes"

    projectLicense =
      "AGPL 3.0" |> url ?~ URL "https://www.gnu.org/licenses/agpl-3.0.en.html"

    blurb =
      "Bootstrapped & distributed backend-as-a-service with user-controlled data"

app :: Swagger -> Swagger
app = makeDocs (Proxy @API.App)
  ["App" |> description ?~ "Hosted applications"]

dns :: Swagger -> Swagger
dns = makeDocs (Proxy @API.DNS)
  ["DNS" |> description ?~ "DNS management"]

user :: Swagger -> Swagger
user = makeDocs (Proxy @API.User)
  ["User" |> description ?~ "Accounts, authentication, and stats"]

heroku :: Swagger -> Swagger
heroku = makeDocs (Proxy @API.Heroku)
  ["Heroku" |> description ?~ "Interaction with the Heroku add-on API"]

ipfs :: Swagger -> Swagger
ipfs = makeDocs (Proxy @API.IPFS)
  ["IPFS" |> description ?~ "The primary IPFS API"]

ping :: Swagger -> Swagger
ping = makeDocs (Proxy @API.Ping)
  ["Ping" |> description ?~ "Check for liveness"]

makeDocs
  :: ( Servant.API.IsSubAPI subRoute Fission.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> [Tag]
  -> Swagger
  -> Swagger
makeDocs routeProxy = applyTagsFor (subOps routeProxy)

subOps
  :: ( Applicative f
     , Servant.API.IsSubAPI subRoute Fission.API
     , HasSwagger subRoute
     )
  => Proxy subRoute
  -> (Operation -> f Operation)
  ->   Swagger
  -> f Swagger
subOps routeProxy = subOperations routeProxy $ Proxy @Fission.API
