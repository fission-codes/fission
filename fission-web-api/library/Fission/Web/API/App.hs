module Fission.Web.API.App
  ( docsV_
  -- , docsV2
  , module Fission.Web.API.App.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.App.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

-- docsV2 :: ("api" :> "v2" :> App) `IsSubAPI` api => Proxy api -> Swagger -> Swagger
-- docsV2 api = applyTagsFor (subOperations (Proxy @App) api) tags'

docsV_ :: App `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docsV_ api = applyTagsFor (subOperations (Proxy @App) api) tags'

tags' :: [Tag]
tags' = ["App" |> description ?~ "Hosted applications"]
