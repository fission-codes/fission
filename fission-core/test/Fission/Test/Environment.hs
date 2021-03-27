module Fission.Test.Environment (spec) where

import           Fission.Environment

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Fission.Environment" do
    describe "Fallback (.!~)" do
      context "wrapped in Right" do
        it "does not fallback when there is a value" $
          let
            result      = Identity (Just 9) .!~ 42
            expectation = Identity 9
          in
            result `shouldBe` (expectation :: Identity Int)

        it "does fallback when there is a no value" $
          let
            result      = Identity Nothing .!~ 42
            expectation = Identity 42
          in
            result `shouldBe` (expectation :: Identity Natural)

      context "wrapped in Left" do
        it "does not fallback when there is a value" $
          let
            result      = Identity (Just 9) .!~ 42
            expectation = Identity 9
          in
            result `shouldBe` (expectation :: Identity Int)

        it "does fallback when there is a no value" $
          let
            result      = Identity Nothing .!~ 42
            expectation = Identity 42
          in
            result `shouldBe` (expectation :: Identity Natural)

    describe "withEnv" do
      it "runs on the fallback when the envar is unset" do
        result <- withEnv "NOT_SET" "my.host" (drop 1)
        result `shouldBe` "my.host"

    describe "withFlag" do
      it "uses the fallback when the envar is not set" do
        result <- withFlag "NOT_SET" "nope" ("yep" :: Text)
        result `shouldBe` "nope"

    describe "getFlag" do
      it "returns Nothing when the flag is not set" do
        result <- getFlag "THIS_KEY_IS_UNSET"
        result `shouldBe` Nothing
