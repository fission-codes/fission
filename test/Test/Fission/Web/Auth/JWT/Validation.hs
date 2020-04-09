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

      -- itsProp "is testing the Cryptonite library" 1 \(Pair pk sk) ->
      --   (RSA.sign Nothing (Just SHA256) sk "hello world")
      --     `shouldBe` Right ""

      --   -- RSA.verify (Just SHA256) pk "hello world" <$> (RSA.sign Nothing (Just SHA256) sk "hello world")
      --   --   `shouldBe` Right True



      context "real world bearer token" do
        it "is valid" do
          JWT.check' Fixture.jwtRSA2048 Fixture.validTime `shouldBe` Right Fixture.jwtRSA2048
