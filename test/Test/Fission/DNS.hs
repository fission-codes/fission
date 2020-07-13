module Test.Fission.DNS (tests) where

import           Test.Fission.Prelude

import qualified Fission.DNS          as DNS
import qualified RIO.Text as Text

tests :: IO TestTree
tests =
  testSpec "Fission.DNS" do
    describe "splitRecord" do
      context "256 characters or less" do
        itsProp' "length does not change" \(SmallText txt) -> 
          DNS.splitRecord txt `shouldBe` pure txt

      -- @@TODO: Add this test back in once the LargeText Arbitrary instance works

      -- context "over 256 characters" do
      --   itsProp "length does not change" 1 \(LargeText txt) -> 
      --     splitCount txt `shouldBe` expectedCount txt

      --     where
      --       splitCount = NonEmpty.length . DNS.splitRecord
      --       expectedCount txt = (Text.length txt `div` 252) + 1

newtype SmallText = SmallText Text
  deriving newtype (Show, Eq)

instance Arbitrary SmallText where
  arbitrary = SmallText . Text.take 256 <$> arbitrary

newtype LargeText = LargeText Text
  deriving newtype (Show, Eq)

instance Arbitrary LargeText where
  arbitrary = do
    len <- arbitrary
    txt <- txtMinLen "" $ 257 + abs len
    return $ LargeText txt

    where
      txtMinLen acc len =
        case Text.compareLength acc len of
          LT -> do
            newTxt <- arbitrary
            txtMinLen (acc <> newTxt) len

          _ -> 
            return acc
