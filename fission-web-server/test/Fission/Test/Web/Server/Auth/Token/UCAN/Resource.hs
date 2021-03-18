module Fission.Test.Web.Server.Auth.Token.UCAN.Resource (spec) where

import qualified Data.Aeson                                 as JSON

import qualified Fission.Internal.UTF8                      as UTF8
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Test.Web.Server.Prelude

spec :: Spec
spec =
  describe "Resource" $ parallel do
    describe "serialization" $ parallel do
      itsProp' "serialized is isomorphic to ADT" \(resource :: Resource) ->
        JSON.eitherDecode (JSON.encode resource) `shouldBe` Right resource
