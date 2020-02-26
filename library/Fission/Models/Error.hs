module Fission.Models.Error
  ( NotFound (..)
  ) where

import Fission.Prelude

data NotFound entity
  = NotFound
  deriving ( Show
           , Eq
           , Exception
           )
