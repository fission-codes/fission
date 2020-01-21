module Test.Fission.Random (tests) where

import qualified RIO.ByteString as BS
import           Test.Tasty.Hspec

import           Test.Fission.Prelude
import qualified Fission.Random as Random

tests :: IO TestTree
tests =
  testSpec "Random" <| parallel do
    describe "bsRandomLength" do
      itsProp "length is the same as specified" 10_000 \n -> do
        alphaNum <- Random.bsRandomLength n

        alphaNum
          |> BS.length
          |> shouldBe (fromIntegral n)
