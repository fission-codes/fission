module Fission.Test.URL.Validation (spec) where

import           Fission.URL.Validation

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "URL.Validation" $ parallel do
    describe "isValid" $ parallel do
      it "is valid on a simple string" $
        isValid "simple" `shouldBe` True

      context "blocklisted words" $ parallel do
        it "is disallowed" do
          isValid "recovery" `shouldBe` False

        it "checks in a case-insensitive way" $
          isValid "reCovErY" `shouldBe` False

      context "characters" $ parallel do
        it "can contain hyphens" $
          isValid "happy-name" `shouldBe` True

        it "can contain underscores" $
          isValid "under_score" `shouldBe` True

        it "does not allow uppercase characters at all" do
          isValid "hElLoWoRlD" `shouldBe` False

        describe "only accepts URL-safe characters" $ parallel do
          it "may not contain a plus sign" $
            isValid "plus+plus" `shouldBe` False

          it "may not contain a space" $
            isValid "with space" `shouldBe` False

          it "may not contain a period" $
            isValid "with.dot" `shouldBe` False

          it "may not contain multiple periods" $
            isValid "has.two.dots" `shouldBe` False

          it "may not contain a mix of invalid characters" $
            isValid "name&with#chars" `shouldBe` False

        context "leads/trailing" $ parallel do
          describe "must start with an alphanumeric character" $ parallel do
            it "may not start with a hyphen" do
              isValid "-startswith" `shouldBe` False

            it "may not start with an underscore" do
              isValid "_startswith" `shouldBe` False

          it "must end with an alphanumeric character" do
            isValid "endswith-" `shouldBe` False
