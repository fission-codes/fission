module Test.Fission.Random (tests) where

import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import           Test.Tasty.Hspec
import           Test.Fission.Prelude

import qualified Fission.Random as Random

tests :: IO TestTree
tests = testSpec "Fission.Random" $ parallel do
  alphaNumSpec
  alphaNumSymbolSpec
  bsRandomLengthSpec

alphaNumSpec :: SpecWith ()
alphaNumSpec =
  describe "alphaNum" do
    itsProp' "length is the specified length" \n -> do
      alphaNum <- Random.alphaNum n
      Text.length alphaNum `shouldBe` fromIntegral n

alphaNumSymbolSpec :: SpecWith ()
alphaNumSymbolSpec =
  describe "alphaNumSymbol" do
    itsProp' "length is the specified length" \n -> do
      alphaNum <- Random.alphaNumSymbol n
      Text.length alphaNum `shouldBe` fromIntegral n

bsRandomLengthSpec :: SpecWith ()
bsRandomLengthSpec =
  describe "bsRandomLength" do
    itsProp' "length is the same as specified" \n -> do
      byteString <- Random.bsRandomLength n

      byteString
        |> BS.length
        |> shouldBe (fromIntegral n)
