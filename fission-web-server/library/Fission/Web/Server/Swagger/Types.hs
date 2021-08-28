module Fission.Web.Server.Swagger.Types (V2, V_, Docs) where

import           Servant.API
import           Servant.Swagger.UI.ReDoc

type V2 = "v2" :> Docs
type V_ = Docs

type Docs = SwaggerSchemaUI "docs" "docs.json"
