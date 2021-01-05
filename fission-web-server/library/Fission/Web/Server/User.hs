module Fission.Web.Server.User
  ( module Fission.Web.Server.User.Retriever
  , module Fission.Web.Server.User.Creator
  , module Fission.Web.Server.User.Modifier
  , module Fission.Web.Server.User.Destroyer
  , module Fission.Web.Server.User.Types
  , module Fission.Web.Server.User.Registration.Types
  , CRUD
  ) where

import           Fission.Web.Server.User.Creator            hiding (Errors')
import           Fission.Web.Server.User.Destroyer
import           Fission.Web.Server.User.Modifier           hiding (Errors')
import           Fission.Web.Server.User.Registration.Types
import           Fission.Web.Server.User.Retriever
import           Fission.Web.Server.User.Types

type CRUD m
  = ( Retriever m
    , Creator   m
    , Modifier  m
    , Destroyer m
    )
