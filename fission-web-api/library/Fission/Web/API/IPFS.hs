module Fission.Web.API.IPFS
  ( docs
  , module Fission.Web.API.IPFS.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.IPFS.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: IPFS `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api =
  applyTagsFor (subOperations (Proxy @IPFS) api)
    ["IPFS" |> description ?~ "The primary IPFS API"]
