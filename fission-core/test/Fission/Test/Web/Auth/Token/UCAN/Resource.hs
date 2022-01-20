module Fission.Test.Web.Auth.Token.UCAN.Resource (spec) where

import qualified Data.Aeson                                       as JSON

import qualified Web.UCAN.Proof.Properties                        as Proof.Properties

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Resource" do

    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(resource :: Resource) ->
        JSON.eitherDecode (JSON.encode resource) `shouldBe` Right resource

    describe "has partial order properties on DelegationSemantics" do

      itsProp' "x canDelegate x"
        (Proof.Properties.reflexive @(Scope Resource))

      itsProp' "if x canDelegate y and y canDelegate x then x == y"
        (Proof.Properties.antisymmetric @(Scope Resource))

      itsProp' "if x canDelegate y and y canDelegate z then x canDelegate z"
        (Proof.Properties.transitive @(Scope Resource))
