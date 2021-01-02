module Test.Fission.DNS (tests) where

import           Test.Fission.Prelude

import qualified Fission.DNS as DNS

import qualified RIO.NonEmpty as NonEmpty
import qualified RIO.Text     as Text


tests :: IO TestTree
tests =
  testSpec "Fission.DNS" do
    describe "splitRecord" do
      context "256 characters or less" do
        itsProp' "length does not change" \(SmallText txt) -> 
          DNS.splitRecord txt `shouldBe` pure txt

      context "over 256 characters" do
        itsProp' "length does not change" \(LargeText txt) -> 
          splitCount txt `shouldBe` expectedCount txt

          where
            splitCount = NonEmpty.length . DNS.splitRecord
            expectedCount txt = (Text.length txt `div` 251) + 1
      
      -- add a serialize/deserialize test once we add `DNS.combineRecords`

newtype SmallText = SmallText Text
  deriving newtype (Show, Eq)

instance Arbitrary SmallText where
  arbitrary = SmallText . Text.take 255 <$> arbitrary

newtype LargeText = LargeText Text
  deriving newtype (Show, Eq)

instance Arbitrary LargeText where
  arbitrary = LargeText . Text.replicate 256 <$> arbitrary
