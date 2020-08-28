module Test.Fission.Web.Auth.Token.UCAN.Resource (tests) where

import qualified Data.Aeson                          as JSON
import qualified Data.ByteString.Lazy.Char8          as Lazy.Char8
import qualified RIO.ByteString.Lazy                 as Lazy

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import           Fission.Web.Auth.Token.JWT

import           Fission.Web.Auth.Token.UCAN.Resource.Types

import qualified Fission.Internal.Fixture.Bearer     as Bearer
import qualified Fission.Internal.UTF8               as UTF8

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "Resource" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(resource :: Resource) ->
        JSON.eitherDecode (JSON.encode resource) `shouldBe` Right resource
