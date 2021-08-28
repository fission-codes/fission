module Fission.Web.API.Heroku
  ( docs
  , module Fission.Web.API.Heroku.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.Heroku.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @Heroku)
  ["Heroku" |> description ?~ "Interaction with the Heroku add-on API"]
