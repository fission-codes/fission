module Fission.Web.Server.App
  ( module Fission.Web.Server.App.Retriever
  , module Fission.Web.Server.App.Creator
  , module Fission.Web.Server.App.Modifier
  , module Fission.Web.Server.App.Destroyer

  , CRUD
  ) where

import           Fission.Web.Server.App.Creator   hiding (Errors')
import           Fission.Web.Server.App.Destroyer hiding (Errors')
import           Fission.Web.Server.App.Modifier  hiding (Errors')
import           Fission.Web.Server.App.Retriever

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )
