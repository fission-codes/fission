module Test.Web.UCAN (spec) where

import qualified Data.Aeson                                as JSON
import Web.UCAN.Types

import           Test.Web.UCAN.Prelude

import Test.Web.UCAN.Example
import qualified Test.Web.UCAN.DelegationSemantics as DelegationSemantics


spec :: Spec
spec =
  describe "Web.UCAN" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(ucan :: UCAN () Resource Potency) ->
        JSON.eitherDecode (JSON.encode ucan) `shouldBe` Right ucan
    
    describe "DelegationSemantics" do
      describe "Resource" do
        DelegationSemantics.partialOrderProperties @Resource
      
      describe "Potency" do
        DelegationSemantics.partialOrderProperties @Potency
      
      describe "Maybe _" do
        DelegationSemantics.partialOrderProperties @(Maybe Potency)


