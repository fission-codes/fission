module Fission.Test.Web.Auth.Token.UCAN.Validation (spec) where

-- import qualified Fission.Internal.Fixture.Bearer       as Fixture
-- import qualified Fission.Internal.Fixture.Bearer.Nested as Nested.Fixture

-- import qualified Fission.Web.Auth.Token.UCAN.Validation as UCAN

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "UCAN Validation" do
    context "RSA 2048" do
      return ()
      -- FIXME when we have a functioning real world case
      -- context "real world bearer token" do
      --   it "is valid" do
      --     UCAN.pureChecks Fixture.rawContent Fixture.jwtRSA2048
      --       `shouldBe` Right Fixture.jwtRSA2048

      -- context "real world nested bearer token -- end to end" do
      --   it "is valid" do
      --     UCAN.check Nested.Fixture.rawContent Nested.Fixture.jwtRSA2048
      --       `shouldBe` Nested.Fixture.InTimeBounds (pure $ Right Nested.Fixture.jwtRSA2048)
