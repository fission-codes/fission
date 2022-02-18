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
      itsProp' "produces parenthood proofs on UCANs without proofs" \(ucan :: UCAN () Capability) -> do
        proofs <- capabilities ucan{ claims = (claims ucan){ proofs = [] } }
        let isParenthoodWitness = \case
              Right ProofParenthood{} -> True
              _                       -> False
        all isParenthoodWitness proofs `shouldBe` True

      itsProp' "produces only valid proofs" \(ucan :: UCAN () Capability) -> do
        proofs <- capabilities ucan
        all
          (\case
            Right proof -> checkProof ucan proof
            Left _      -> True
          ) proofs `shouldBe` True

