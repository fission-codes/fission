module Fission.IPFS.Error
  ( Add (..)
  , Error (..)
  , Linearization (..)
  ) where

import RIO

import Fission.IPFS.Types

data Error
  = AddErr Add
  | LinearizationErr Linearization
  deriving ( Eq
           , Show
           )

data Add
  = InvalidFile
  | UnexpectedOutput Text
  | UnknownError
  deriving ( Eq
           , Show
           )

data Linearization
  = NonLinear SparseTree
  deriving ( Eq
           , Show
           )
