module Fission.Web.Server.Internal.Orphanage.SwaggerTag () where

import           Data.Proxy
import           Data.Swagger

import           Servant.API
import           Servant.Server
import           Servant.Swagger

import           Fission.Web.API.Doc.Types

instance HasServer api ctx => HasServer (SwaggerTag name description :> api) ctx where
  type ServerT (SwaggerTag name description :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)
