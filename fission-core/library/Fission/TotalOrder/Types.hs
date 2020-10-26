module Fission.TotalOrder.Types (TotalOrder (..)) where

import           RIO

import           Fission.PartialOrder.Class

newtype TotalOrder a = TotalOrder a

instance Eq a => Eq (TotalOrder a) where
  TotalOrder a == TotalOrder b = a == b

instance Ord a => Ord (TotalOrder a) where
  TotalOrder a `compare` TotalOrder b = a `compare` b

instance Ord a => PartialOrder (TotalOrder a) where
  relationship (TotalOrder a) (TotalOrder b) =
    case compare a b of
      EQ -> Equal
      LT -> Descendant
      GT -> Ancestor
