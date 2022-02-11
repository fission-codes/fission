{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Witness.Class
  ( DelegationSemantics(..)
  , GreaterDelegatesMore(..)
  , SmallerDelegatesMore(..)
  , EqualCanDelegate(..)
  ) where

import           Data.Coerce
import           RIO

class DelegationSemantics rsc where
  canDelegate :: rsc -> rsc -> Bool

instance DelegationSemantics rsc => DelegationSemantics (Maybe rsc) where
  Nothing           `canDelegate` Nothing    = True
  Nothing           `canDelegate` _          = False
  _                 `canDelegate` Nothing    = True
  (Just rscWitness) `canDelegate` (Just rsc) = rscWitness `canDelegate` rsc

-- For deriving via

newtype GreaterDelegatesMore a = GreaterDelegatesMore a
newtype SmallerDelegatesMore a = SmallerDelegatesMore a
newtype EqualCanDelegate a = EqualCanDelegate a

instance Ord a => DelegationSemantics (GreaterDelegatesMore a) where
  canDelegate = coerce @(a -> a -> Bool) (>=)

instance Ord a => DelegationSemantics (SmallerDelegatesMore a) where
  canDelegate = coerce @(a -> a -> Bool) (<=)

instance Eq a => DelegationSemantics (EqualCanDelegate a) where
  canDelegate = coerce @(a -> a -> Bool) (==)
