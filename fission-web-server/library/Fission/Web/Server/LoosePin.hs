module Fission.Web.Server.LoosePin
  ( module Fission.Web.Server.LoosePin.Retriever
  , module Fission.Web.Server.LoosePin.Creator
  , module Fission.Web.Server.LoosePin.Destroyer
  , CRUD
  ) where

import           Fission.Web.Server.LoosePin.Creator
import           Fission.Web.Server.LoosePin.Destroyer
import           Fission.Web.Server.LoosePin.Retriever

type CRUD m
  = ( Retriever m
    , Creator   m
    , Destroyer m
    )
