module Fission.Web.API.User
  ( docs
  , module Fission.Web.API.User.Types
  ) where

import  Fission.Web.API.Prelude
import  Fission.Web.API.User.Types

docs :: Swagger -> Swagger
docs = makeDocs (Proxy @User)
  ["User" |> description ?~ "Accounts, authentication, and stats"]
