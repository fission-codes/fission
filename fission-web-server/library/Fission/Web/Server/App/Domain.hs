module Fission.Web.Server.App.Domain
  ( module Fission.Web.Server.App.Domain.Error

  , module Fission.Web.Server.App.Domain.Initializer
  , module Fission.Web.Server.App.Domain.Associator
  , module Fission.Web.Server.App.Domain.Dissociator
  , module Fission.Web.Server.App.Domain.Retriever

  , Actions
  ) where

import           Fission.Web.Server.App.Domain.Error

import           Fission.Web.Server.App.Domain.Associator
import           Fission.Web.Server.App.Domain.Dissociator (Dissociator (..))
import           Fission.Web.Server.App.Domain.Initializer
import           Fission.Web.Server.App.Domain.Retriever   (Retriever (..))

type Actions m
  = ( Initializer m
    , Associator  m
    , Dissociator m
    , Retriever   m
    )
