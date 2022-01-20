module Fission.Test.Web.Auth.Token.UCAN.Resource.Scope (spec) where

import qualified Data.Aeson                                       as JSON

import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Resource Scope" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(scope :: Scope Text) ->
        scope /= Subset "*" ==> -- JSON.encode Complete == JSON.encode (Subset "*"). So we filter it out
          JSON.eitherDecode (JSON.encode scope) `shouldBe` Right scope
