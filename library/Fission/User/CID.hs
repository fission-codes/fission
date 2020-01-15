module Fission.User.CID
  ( module Fission.User.CID.Retriever
  , module Fission.User.CID.Creator
  , module Fission.User.CID.Destroyer
  , CRUD
  ) where

import Fission.User.CID.Retriever
import Fission.User.CID.Creator
import Fission.User.CID.Destroyer

type CRUD m
  = ( Retriever m
    , Creator   m
    , Destroyer m
    )
