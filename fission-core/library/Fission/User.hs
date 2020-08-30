module Fission.User
  ( module Fission.User.Retriever
  , module Fission.User.Creator
  , module Fission.User.Modifier
  , module Fission.User.Destroyer
  , module Fission.User.Types
  , module Fission.User.Registration.Types
  , CRUD
  ) where

import           Fission.User.Creator            hiding (Errors')
import           Fission.User.Destroyer
import           Fission.User.Modifier           hiding (Errors')
import           Fission.User.Registration.Types
import           Fission.User.Retriever
import           Fission.User.Types

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )
