module Test.Web.UCAN.Attenuation (spec) where

import           Web.UCAN.Capabilities
import           Web.UCAN.Types

import           Test.Prelude

import           Test.Web.UCAN.Example
import           Test.Web.UCAN.Orphanage.DummyResolver ()


spec :: Spec
spec =
  describe "Attenuation" do
    describe "capabilities" do
      itsProp' "produces parenthood witnesses on UCANs without proofs" \(ucan :: UCAN () Capability) -> do
        witnesses <- capabilities ucan{ claims = (claims ucan){ proofs = [] } }
        let isParenthoodWitness = \case
              WitnessParenthood{} -> True
              _                   -> False
        all isParenthoodWitness witnesses `shouldBe` True

      itsProp' "produces only valid witnesses" \(ucan :: UCAN () Capability) -> do
        witnesses <- capabilities ucan
        all (checkWitness ucan) witnesses `shouldBe` True

