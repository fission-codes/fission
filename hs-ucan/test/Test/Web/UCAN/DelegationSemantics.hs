{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Web.UCAN.DelegationSemantics
  ( reflexive
  , antisymmetric
  , transitive
  , partialOrderProperties
  ) where

import           Test.Web.UCAN.Prelude

import           Web.UCAN.Proof.Class


reflexive :: DelegationSemantics res => res -> Bool
reflexive res =
  res `canDelegate` res

antisymmetric :: (DelegationSemantics res, Eq res ) => res -> res -> Property
antisymmetric res0 res1 =
  ((res0 `canDelegate` res1) && (res1 `canDelegate` res0)) ==> (res0 == res1)

transitive :: DelegationSemantics res => res -> res -> res -> Property
transitive res0 res1 res2 =
  (res0 `canDelegate` res1) && (res1 `canDelegate` res2) ==> res0 `canDelegate` res2

partialOrderProperties ::
  forall res .
  ( Eq res
  , DelegationSemantics res
  , Arbitrary res
  , Show res
  )
  => Spec
partialOrderProperties = do
  describe "has partial order properties on DelegationSemantics" do

    itsProp' "x canDelegate x"
      (reflexive @res)

    itsProp' "if x canDelegate y and y canDelegate x then x == y"
      (antisymmetric @res)

    itsProp' "if x canDelegate y and y canDelegate z then x canDelegate z"
      (transitive @res)
