module Fission.Test.Internal.Bool (spec) where

import           Fission.Internal.Bool

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Fission.Internal.Bool" do
    describe "truthy" do
      context "t" $
        it "is truthy" $ truthy "t" `shouldBe` True

      context "f" $
        it "is not truthy" $ truthy "f" `shouldBe` False

      context "on" $
        it "is truthy" $ truthy ("on" :: Text) `shouldBe` True

      context "no" $
        it "is not truthy" $ truthy ("no" :: ByteString) `shouldBe` False
