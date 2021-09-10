module Fission.Web.API.Docs.Types (Docs) where

import           Servant.Swagger.UI.Core

type Docs
  =  Summary "Docs"
  :> Description "Swagger-based documentation"
  :> SwaggerSchemaUI "docs" "docs.json"
