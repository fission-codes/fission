module Fission.Web.API.App
  ( docs
  , module Fission.Web.API.App.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.App.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @API.App)
  ["App" |> description ?~ "Hosted applications"]
