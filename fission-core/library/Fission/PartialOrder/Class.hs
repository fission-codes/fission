{-# LANGUAGE UndecidableInstances #-}

module Fission.PartialOrder.Class
  ( PartialOrder (..)
  , module Fission.PartialOrder.Types
  ) where

import           RIO

import           Fission.PartialOrder.Types

class PartialOrder a where
  relationship :: a -> a -> Relationship

-- instance Ord a => PartialOrder a where
--   relationship x y =
--     case compare x y of
--       EQ -> Equal
--       LT -> Descendant
--       GT -> Ancestor
