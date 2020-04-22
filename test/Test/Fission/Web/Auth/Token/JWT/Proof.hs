module Test.Fission.Web.Auth.Token.JWT.Proof (tests) where

import qualified Data.Aeson as JSON
 
import           Fission.Web.Auth.Token.JWT

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Proof" do
    describe "serialization" do
      itsProp' "serialize+deserialize is the identity function" \(proof :: Proof) ->
        JSON.eitherDecode (JSON.encode proof) `shouldBe` Right proof
