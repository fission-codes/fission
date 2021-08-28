module Fission.Web.Server.Swagger (handler) where

import           Data.Version

import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI.ReDoc

import           Fission.Prelude

import qualified Fission.Web.API.Docs             as Docs
import qualified Fission.Web.API.Host.Types       as Web

import qualified Fission.Web.Server.Swagger.Types as Swagger

handler ::
  forall api m .
  HasSwagger api
  => (forall a . Handler a -> m a)
  -> Web.Host
  -> Version
  -> Proxy api
  -> ServerT Swagger.Docs m
handler fromHandler appHost v pxy =
  appHost
    |> Docs.meta pxy v
    |> redocSchemaUIServer
    |> hoistServer (Proxy @Swagger.Docs) fromHandler
