module Test.Fission.Web.Auth.JWT.Validation (tests) where

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer
import qualified Fission.Internal.UTF8               as UTF8

import qualified Fission.Internal.Fixture.Bearer as Fixture
import qualified Fission.Web.Auth.JWT.Validation as JWT

import           Test.Fission.Prelude

import Fission.Internal.RSA2048.Pair.Types

import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import           Crypto.Hash.Algorithms (SHA256 (..))

tests :: SpecWith ()
tests =
  describe "JWT Validation" do
    context "RSA 2048" do
      context "real world bearer token" do
        it "is valid" do
          JWT.check' Fixture.jwtRSA2048 Fixture.validTime `shouldBe` Right Fixture.jwtRSA2048
