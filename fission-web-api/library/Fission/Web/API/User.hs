module Fission.Web.API.User
  ( docs
  , module Fission.Web.API.User.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.User.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: User `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api =
  applyTagsFor (subOperations (Proxy @User) api)
    ["User" |> description ?~ "Accounts, authentication, and stats"]
