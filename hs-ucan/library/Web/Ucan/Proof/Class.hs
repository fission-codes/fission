module Web.Ucan.Proof.Class
  ( ResourceSemantics(..)
  ) where

import           RIO

class ResourceSemantics rsc where
  canDelegate :: rsc -> rsc -> Bool

instance ResourceSemantics rsc => ResourceSemantics (Maybe rsc) where
  Nothing         `canDelegate` _          = False
  _               `canDelegate` Nothing    = True
  (Just rscProof) `canDelegate` (Just rsc) = rscProof `canDelegate` rsc
