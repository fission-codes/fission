module Fission.Web.API.Auth
  ( docs
  , module Fission.Web.API.Auth.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.Auth.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @Auth)
  ["Auth" |> description ?~ "Specialized auth endpoints"]
