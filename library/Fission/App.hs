module Fission.App
  ( module Fission.App.Retriever
  , module Fission.App.Creator
  , module Fission.App.Modifier
  , module Fission.App.Destroyer

  , CRUD
  ) where

import Fission.App.Retriever (Retriever (..))
import Fission.App.Creator   (Creator   (..))
import Fission.App.Modifier  (Modifier  (..))
import Fission.App.Destroyer (Destroyer (..))

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )
