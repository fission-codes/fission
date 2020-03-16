module Fission.App
  ( module Fission.App.Retriever
  , module Fission.App.Creator
  , module Fission.App.Modifier
  , module Fission.App.Destroyer

  , module Fission.App.Content
  , module Fission.App.Domain

  , CRUD
  ) where

import Fission.App.Retriever
import Fission.App.Creator
import Fission.App.Modifier
import Fission.App.Destroyer

import Fission.App.Content.Initial
import Fission.App.Domain.Initial

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )

type Defaults m
  = ( Content m
    , Domain  m
    )
