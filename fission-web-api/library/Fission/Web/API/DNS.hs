module Fission.Web.API.DNS
  ( docs
  , module Fission.Web.API.DNS.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.DNS.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: DNS `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api =
  applyTagsFor (subOperations (Proxy @DNS) api)
    ["DNS" |> description ?~ "DNS management"]
