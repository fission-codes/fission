module Fission.Web.API.Auth
  ( docs
  , module Fission.Web.API.Auth.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.Auth.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: ("api" :> "v2" :> Auth) `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api = applyTags tags'

docsV_ :: Auth `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docsV_ api = applyTagsFor (subOperations (Proxy @Auth) api) tags'

tags' :: [Tag]
tags' = ["Auth" |> description ?~ "Specialized auth endpoints"]
