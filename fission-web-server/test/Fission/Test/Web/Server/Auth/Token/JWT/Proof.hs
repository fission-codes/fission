module Fission.Test.Web.Server.Auth.Token.JWT.Proof (spec) where

import qualified Data.Aeson                      as JSON

import           Fission.Web.Auth.Token.JWT

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "Proof" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(proof :: Proof) ->
        JSON.eitherDecode (JSON.encode proof) `shouldBe` Right proof
