module Web.UCAN.Proof.Class
  ( DelegationSemantics(..)
  ) where

import           RIO

class DelegationSemantics rsc where
  canDelegate :: rsc -> rsc -> Bool

instance DelegationSemantics rsc => DelegationSemantics (Maybe rsc) where
  Nothing         `canDelegate` _          = False
  _               `canDelegate` Nothing    = True
  (Just rscProof) `canDelegate` (Just rsc) = rscProof `canDelegate` rsc
