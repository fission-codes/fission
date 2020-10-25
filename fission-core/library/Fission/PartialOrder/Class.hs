{-# LANGUAGE UndecidableInstances #-}

module Fission.PartialOrder.Class
  ( PartialOrder (..)
  , module Fission.PartialOrder.Types
  ) where

import           Fission.Prelude

import           Fission.PartialOrder.Types

class Eq a => PartialOrder a where
  relationship :: a -> a -> Relationship

instance (Eq a, Ord a) => PartialOrder a where
  relationship x y =
    case compare x y of
      EQ -> Equal
      LT -> Descendant
      GT -> Ancestor
