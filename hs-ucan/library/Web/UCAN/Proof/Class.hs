module Web.UCAN.Proof.Class
  ( DelegationSemantics(..)
  , GreaterDelegatesMore(..)
  , SmallerDelegatesMore(..)
  ) where

import           RIO

class DelegationSemantics rsc where
  canDelegate :: rsc -> rsc -> Bool

instance DelegationSemantics rsc => DelegationSemantics (Maybe rsc) where
  Nothing         `canDelegate` Nothing    = True
  Nothing         `canDelegate` _          = False
  _               `canDelegate` Nothing    = True
  (Just rscProof) `canDelegate` (Just rsc) = rscProof `canDelegate` rsc

-- For deriving via

newtype GreaterDelegatesMore a = GreaterDelegatesMore a
newtype SmallerDelegatesMore a = SmallerDelegatesMore a
  
instance Ord a => DelegationSemantics (GreaterDelegatesMore a) where
  (GreaterDelegatesMore a) `canDelegate` (GreaterDelegatesMore b) = a >= b

instance Ord a => DelegationSemantics (SmallerDelegatesMore a) where
  (SmallerDelegatesMore a) `canDelegate` (SmallerDelegatesMore b) = a <= b
