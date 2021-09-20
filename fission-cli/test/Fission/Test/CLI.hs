module Fission.Test.CLI where

import           Fission.Test.CLI.Prelude

spec :: Spec
spec =
  describe "Fission.CLI" do
    it "runs at all" do
      1 `shouldNotBe` 2
