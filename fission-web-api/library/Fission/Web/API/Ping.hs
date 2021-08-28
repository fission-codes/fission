module Fission.Web.API.Ping
  ( docs
  , module Fission.Web.API.Ping.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.Ping.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: Ping `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api =
  applyTagsFor (subOperations (Proxy @Ping) api)
    ["Ping" |> description ?~ "Check for liveness"]
