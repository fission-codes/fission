module Test.Fission.Web.Auth.Token.JWT.Validation (tests) where

import qualified Fission.Internal.Fixture.Bearer        as Fixture
import qualified Fission.Internal.Fixture.Bearer.Nested as Nested.Fixture

import qualified Fission.Web.Auth.Token.JWT.Validation as JWT

import           Test.Fission.Prelude

tests :: SpecWith ()
tests =
  describe "JWT Validation" do
    context "RSA 2048" do
      return ()
      -- FIXME when we have a functioning real world case
      -- context "real world bearer token" do
      --   it "is valid" do
      --     JWT.pureChecks Fixture.rawContent Fixture.jwtRSA2048
      --       `shouldBe` Right Fixture.jwtRSA2048
 
      -- context "real world nested bearer token -- end to end" do
      --   it "is valid" do
      --     JWT.check Nested.Fixture.rawContent Nested.Fixture.jwtRSA2048
      --       `shouldBe` Nested.Fixture.InTimeBounds (pure $ Right Nested.Fixture.jwtRSA2048)
