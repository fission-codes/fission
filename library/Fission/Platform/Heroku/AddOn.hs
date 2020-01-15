module Fission.Platform.Heroku.AddOn
  ( module Fission.Platform.Heroku.AddOn.Creator
  , module Fission.Platform.Heroku.AddOn.Destroyer
  , module Fission.Platform.Heroku.AddOn.Retriever
  , CRUD
  ) where

import Fission.Platform.Heroku.AddOn.Creator
import Fission.Platform.Heroku.AddOn.Destroyer
import Fission.Platform.Heroku.AddOn.Retriever

type CRUD m
  = ( Creator   m
    , Destroyer m
    , Retriever m
    )
