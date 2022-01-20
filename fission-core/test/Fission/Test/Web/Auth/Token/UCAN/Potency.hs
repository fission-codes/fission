module Fission.Test.Web.Auth.Token.UCAN.Potency (spec) where

import qualified Data.Aeson                                as JSON

import           Web.UCAN.Proof.Class
import qualified Web.UCAN.Proof.Properties                 as Proof.Properties

import           Fission.Web.Auth.Token.UCAN.Potency.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Potency" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(potency :: Potency) -> do
        JSON.eitherDecode (JSON.encode potency) `shouldBe` Right potency

    describe "order" do
      it "SuperUser canDelegate Destructive" do
        SuperUser `canDelegate` Destructive `shouldBe` True

      it "Destructive canDelegate AppendOnly" do
        Destructive `canDelegate` AppendOnly `shouldBe` True

    describe "has partial order properties on DelegationSemantics" do

      itsProp' "x canDelegate x"
        (Proof.Properties.reflexive @(Maybe Potency))

      itsProp' "if x canDelegate y and y canDelegate x then x == y"
        (Proof.Properties.antisymmetric @(Maybe Potency))

      itsProp' "if x canDelegate y and y canDelegate z then x canDelegate z"
        (Proof.Properties.transitive @(Maybe Potency))

