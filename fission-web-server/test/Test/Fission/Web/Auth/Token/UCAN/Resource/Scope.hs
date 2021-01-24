module Test.Fission.Web.Auth.Token.UCAN.Resource.Scope (tests) where

import qualified Data.Aeson                                       as JSON

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Resource Scope" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(scope :: Scope Text) ->
        JSON.eitherDecode (JSON.encode scope) `shouldBe` Right scope
