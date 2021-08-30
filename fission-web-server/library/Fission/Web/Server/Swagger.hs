module Fission.Web.Server.Swagger (handler) where

import           Data.Version

import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI.ReDoc

import           Fission.Prelude

import           Fission.Web.API.Docs       as Docs
import qualified Fission.Web.API.Host.Types as Web

handler ::
  forall m api .
  HasSwagger api
  => (forall a . Handler a -> m a)
  -> Web.Host
  -> Version
  -> Proxy api
  -> ServerT Docs m
handler fromHandler appHost v pxy =
  hoistServer (Proxy @Docs) fromHandler . redocSchemaUIServer $ Docs.meta pxy v appHost
