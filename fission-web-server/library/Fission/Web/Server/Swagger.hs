module Fission.Web.Server.Swagger (handler, v_, v2) where

import           Data.Swagger
import           Data.Version
import qualified RIO.Text                                            as Text

import           Servant
import           Servant.Swagger
import qualified Servant.Swagger.Internal.TypeLevel.API              as Servant.API
import           Servant.Swagger.UI.ReDoc

import           Fission.Prelude

import qualified Fission.Web.API.Types                               as Fission

import qualified Fission.Web.Server.Host.Types                       as Web
import qualified Fission.Web.Server.Swagger.Types                    as Swagger

import           Fission.Web.Server.Internal.Orphanage.BasicAuth     ()
import           Fission.Web.Server.Internal.Orphanage.BasicAuthData ()
import           Fission.Web.Server.Internal.Orphanage.HigherOrder   ()
import           Fission.Web.Server.Internal.Orphanage.RegisterDid   ()

import qualified Paths_fission_web_server                            as Fission

handler ::
     (forall a . Handler a -> m a)
  -> Web.Host
  -> (Swagger -> Swagger)
  -> Proxy api
  -> ServerT api m
handler fromHandler docs appHost pxy =
  appHost
    |> fission pxy
    |> redocSchemaUIServer
    |> hoistServer pxy fromHandler
