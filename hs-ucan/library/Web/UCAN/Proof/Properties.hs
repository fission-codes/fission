module Web.UCAN.Proof.Properties
  ( reflexive
  , antisymmetric
  , transitive
  ) where

import           RIO
import           Web.UCAN.Proof.Class


reflexive :: DelegationSemantics res => res -> Bool
reflexive res =
  res `canDelegate` res

antisymmetric :: (DelegationSemantics res, Eq res) => res -> res -> Bool
antisymmetric res0 res1 =
  ((res0 `canDelegate` res1) && (res1 `canDelegate` res0)) `implies` (res0 == res1)

transitive :: DelegationSemantics res => res -> res -> res -> Bool
transitive res0 res1 res2 =
  ((res0 `canDelegate` res1) && (res1 `canDelegate` res2)) `implies` (res0 `canDelegate` res2)

implies :: Bool -> Bool -> Bool
implies a b =
  (a && b) || not a
