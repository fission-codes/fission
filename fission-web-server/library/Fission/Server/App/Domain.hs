module Fission.App.Domain
  ( module Fission.App.Domain.Error

  , module Fission.App.Domain.Initializer
  , module Fission.App.Domain.Associator
  , module Fission.App.Domain.Dissociator
  , module Fission.App.Domain.Retriever

  , Actions
  ) where

import           Fission.App.Domain.Error

import           Fission.App.Domain.Associator
import           Fission.App.Domain.Dissociator (Dissociator (..))
import           Fission.App.Domain.Initializer
import           Fission.App.Domain.Retriever   (Retriever (..))

type Actions m
  = ( Initializer m
    , Associator  m
    , Dissociator m
    , Retriever   m
    )
