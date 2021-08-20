module Main (main) where

import           Crypto.Hash.SpiralRatchet
import           Crypto.Hash.SpiralRatchet.Types

import           Test.Tasty
import           Test.Tasty.Hspec

import           RIO

main :: IO ()
main = do
  spec <- testSpecs $
    describe "SpiralRatchet" do
      describe "generate" do
        it "isn't blank" do
          spiral <- gen
          show spiral `shouldNotBe` ""

      describe "step" do
        it "changes" do
          SpiralRatchet {small} <- gen
          step small `shouldNotBe` small

      describe "advance" do
        it "advances" do
          spiral <- gen
          advance spiral `shouldNotBe` spiral

  defaultMain $ testGroup "Tests" spec
