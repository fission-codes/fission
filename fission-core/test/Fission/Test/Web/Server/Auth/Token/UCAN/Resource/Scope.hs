module Fission.Test.Web.Server.Auth.Token.UCAN.Resource.Scope (spec) where

import qualified Data.Aeson                                       as JSON

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Resource Scope" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(scope :: Scope Text) ->
        JSON.eitherDecode (JSON.encode scope) `shouldBe` Right scope
