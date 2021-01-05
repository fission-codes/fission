module Fission.Web.Server.Heroku.AddOn
  ( module Fission.Web.Server.Heroku.AddOn.Creator
  , module Fission.Web.Server.Heroku.AddOn.Destroyer
  , module Fission.Web.Server.Heroku.AddOn.Retriever
  , CRUD
  ) where

import           Fission.Web.Server.Heroku.AddOn.Creator
import           Fission.Web.Server.Heroku.AddOn.Destroyer
import           Fission.Web.Server.Heroku.AddOn.Retriever

type CRUD m
  = ( Creator   m
    , Destroyer m
    , Retriever m
    )
