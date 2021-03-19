module Fission.Test.URL (spec) where

import           Fission.Internal.URL

import           Fission.Test.Prelude
import qualified Fission.Test.URL.Validation as URL.Validation

spec :: Spec
spec =
  describe "URL" do
    URL.Validation.spec

    describe "isURLCharacter" do
      it "passes for simple alphanumeric Word8s" $
        isURLCharacter (fromChar 'a') `shouldBe` True

      it "fails for slashes" $
        isURLCharacter (fromChar '/') `shouldBe` False

      it "fails for question marks" $
        isURLCharacter (fromChar '?') `shouldBe` False

fromChar :: Char -> Word8
fromChar = ord .> fromIntegral
