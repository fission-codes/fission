module Fission.Web.API.Ping
  ( docs
  , module Fission.Web.API.Ping.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.Ping.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @Ping)
  ["Ping" |> description ?~ "Check for liveness"]
