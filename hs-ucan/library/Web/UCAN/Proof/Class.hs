{-# LANGUAGE ScopedTypeVariables #-}
module Web.UCAN.Proof.Class
  ( DelegationSemantics(..)
  , GreaterDelegatesMore(..)
  , SmallerDelegatesMore(..)
  , EqualCanDelegate(..)
  ) where

import           Data.Coerce
import           RIO

class DelegationSemantics cap where
  canDelegate :: cap -> cap -> Bool

instance DelegationSemantics cap => DelegationSemantics (Maybe cap) where
  Nothing           `canDelegate` Nothing    = True
  Nothing           `canDelegate` _          = False
  _                 `canDelegate` Nothing    = True
  (Just capWitness) `canDelegate` (Just cap) = capWitness `canDelegate` cap

instance (DelegationSemantics a, DelegationSemantics b) => DelegationSemantics (a, b) where
  (a, b) `canDelegate` (c, d) = a `canDelegate` c && b `canDelegate` d


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
