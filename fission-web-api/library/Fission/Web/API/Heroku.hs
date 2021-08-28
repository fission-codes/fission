module Fission.Web.API.Heroku
  ( docs
  , module Fission.Web.API.Heroku.Types
  ) where

import           Data.Swagger
import           Servant.Swagger

import           Fission.Web.API.Heroku.Types
import           Servant.Swagger.TypeLevel

import           Fission.Web.API.Prelude

docs :: Heroku `IsSubAPI` api => Proxy api -> Swagger -> Swagger
docs api =
  applyTagsFor (subOperations (Proxy @Heroku) api)
    ["Heroku" |> description ?~ "Interaction with the Heroku add-on API"]
