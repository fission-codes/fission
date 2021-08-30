module Fission.Web.API.Docs.Types (Docs) where

import           Servant.Swagger.UI.Core

type Docs = SwaggerSchemaUI "docs" "docs.json"
