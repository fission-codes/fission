module Fission.Web.API.IPFS
  ( docs
  , module Fission.Web.API.IPFS.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.IPFS.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @IPFS)
  ["IPFS" |> description ?~ "The primary IPFS API"]
