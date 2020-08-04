module Fission.App
  ( module Fission.App.Retriever
  , module Fission.App.Creator
  , module Fission.App.Modifier
  , module Fission.App.Destroyer

  , CRUD
  ) where

import           Fission.App.Creator   hiding (Errors')
import           Fission.App.Destroyer hiding (Errors')
import           Fission.App.Modifier  hiding (Errors')
import           Fission.App.Retriever

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )
