module Fission.Test.Web.Auth.Token.UCAN.Proof (spec) where

import qualified Data.Aeson                        as JSON

import           Fission.Web.Auth.Token.UCAN.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Proof" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(proof :: Proof) ->
        JSON.eitherDecode (JSON.encode proof) `shouldBe` Right proof
