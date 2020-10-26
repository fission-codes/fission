{-# LANGUAGE UndecidableInstances #-}

module Fission.PartialOrder.Class
  ( PartialOrder (..)
  , module Fission.PartialOrder.Types
  ) where

import           Fission.PartialOrder.Types

class PartialOrder a where
  relationship :: a -> a -> Relationship
