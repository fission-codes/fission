module Test.Fission.Random (tests) where

import qualified RIO.ByteString as BS
import qualified RIO.Text       as Text

import           Test.Tasty.Hspec
import           Test.Fission.Prelude

import qualified Fission.Random as Random

tests :: IO TestTree
tests = testSpec "Random" <| parallel do
  alphaNumSpec
  alphaNumSymbolSpec
  bsRandomLengthSpec

alphaNumSpec :: SpecWith ()
alphaNumSpec =
  describe "alphaNum" do
    describe "length" do
      itsProp' "is at most what is specified" \n -> do
        alphaNum <- Random.alphaNum n

        Text.length alphaNum `shouldSatisfy` \actual ->
          fromIntegral actual <= n

      itsProp' "is not zero unless specified" \n -> do
        alphaNum <- Random.alphaNum n

        unless (n == 0) do
          Text.length alphaNum `shouldSatisfy` (> 0)

alphaNumSymbolSpec :: SpecWith ()
alphaNumSymbolSpec =
  describe "alphaNumSymbol" do
    describe "length" do
      itsProp' "is at most what is specified" \n -> do
        alphaNumSymbol <- Random.alphaNumSymbol n

        Text.length alphaNumSymbol `shouldSatisfy` \actual ->
          fromIntegral actual <= n

      itsProp' "is not zero unless specified" \n -> do
        alphaNumSymbol <- Random.alphaNumSymbol n

        unless (n == 0) do
          Text.length alphaNumSymbol `shouldSatisfy` (> 0)

bsRandomLengthSpec :: SpecWith ()
bsRandomLengthSpec =
  describe "bsRandomLength" do
    itsProp' "length is the same as specified" \n -> do
      byteString <- Random.bsRandomLength n

      byteString
        |> BS.length
        |> shouldBe (fromIntegral n)
