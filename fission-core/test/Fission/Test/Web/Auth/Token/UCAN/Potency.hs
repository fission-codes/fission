module Fission.Test.Web.Auth.Token.UCAN.Potency (spec) where

import qualified Data.Aeson                                as JSON
import           Data.Bits                                 (xor)

import           Web.UCAN.Proof.Class

import           Fission.Web.Auth.Token.UCAN.Potency.Types

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Potency" do
    describe "serialization" do
      itsProp' "serialized is isomorphic to ADT" \(potency :: Potency) -> do
        JSON.eitherDecode (JSON.encode potency) `shouldBe` Right potency

    describe "order" do
      it "SuperUser canDelegate Destructive" do
        SuperUser `canDelegate` Destructive `shouldBe` True

      it "Destructive canDelegate AppendOnly" do
        Destructive `canDelegate` AppendOnly `shouldBe` True

    describe "properties" do
      itsProp' "potency can't delegate out of thin air" \(potency :: Potency) -> do
        Nothing `canDelegate` Just potency `shouldBe` False

      itsProp' "potency can delegate is reflexive" \(potency :: Potency) -> do
        potency `canDelegate` potency `shouldBe` True

      itsProp' "reverses sign when swapped, except when equal" \((ptc1, ptc2) :: (Potency, Potency)) -> do
        -- "xor (ptc1 != ptc2)" means "swap sign, if they're not equal"
        ptc1 `canDelegate` ptc2 `shouldBe` ((ptc2 `canDelegate` ptc1) `xor` (ptc1 /= ptc2))
