module Fission.Test.Random (spec) where

import qualified RIO.ByteString       as BS
import qualified RIO.Text             as Text

import qualified Fission.Random       as Random

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Random" do
    describe "alphaNum" do
      itsProp' "length is the specified length" \n -> do
        alphaNum <- Random.alphaNum n
        Text.length alphaNum `shouldBe` fromIntegral n

    describe "alphaNumSymbol" do
      itsProp' "length is the specified length" \n -> do
        alphaNum <- Random.alphaNumSymbol n
        Text.length alphaNum `shouldBe` fromIntegral n

    describe "bsRandomLength" do
      itsProp' "length is the same as specified" \n -> do
          byteString <- Random.bsRandomLength n
          BS.length byteString `shouldBe` fromIntegral n
