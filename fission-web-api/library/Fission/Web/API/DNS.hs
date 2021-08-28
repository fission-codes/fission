module Fission.Web.API.DNS
  ( docs
  , module Fission.Web.API.DNS.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.DNS.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @DNS)
  ["DNS" |> description ?~ "DNS management"]
