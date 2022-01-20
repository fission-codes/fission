module Web.UCAN.Test (spec) where

import           Web.UCAN.Test.Prelude


spec :: Spec
spec =
  describe "Web.UCAN" do
    it "runs at all" do
      1 `shouldNotBe` 2



