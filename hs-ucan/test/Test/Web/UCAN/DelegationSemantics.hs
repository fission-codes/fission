{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Web.UCAN.DelegationSemantics
  ( reflexive
  , antisymmetric
  , transitive
  , itHasPartialOrderProperties
  ) where

import           Test.Prelude

import           Web.UCAN.Witness.Class
import           Web.UCAN.Witness.Properties


itHasPartialOrderProperties ::
  forall res .
  ( Eq res
  , DelegationSemantics res
  , Arbitrary res
  , Show res
  )
  => Spec
itHasPartialOrderProperties = do
  describe "has partial order properties on DelegationSemantics" do

    itsProp' "x canDelegate x" $
      reflexive @res

    itsProp' "if x canDelegate y and y canDelegate x then x == y" $
      antisymmetric @res

    itsProp' "if x canDelegate y and y canDelegate z then x canDelegate z" $
      transitive @res
