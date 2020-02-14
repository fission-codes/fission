module Fission.LoosePin
  ( module Fission.LoosePin.Retriever
  , module Fission.LoosePin.Creator
  , module Fission.LoosePin.Destroyer
  , CRUD
  ) where

import Fission.LoosePin.Retriever
import Fission.LoosePin.Creator
import Fission.LoosePin.Destroyer

type CRUD m
  = ( Retriever m
    , Creator   m
    , Destroyer m
    )
