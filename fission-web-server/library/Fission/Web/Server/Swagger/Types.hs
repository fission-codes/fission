module Fission.Web.Server.Swagger.Types (API) where

import           Servant.Swagger.UI.ReDoc

type API = SwaggerSchemaUI "docs" "docs.json"
