module Test.Fission.User.DID (tests) where

import           Test.Fission.Prelude

import           Fission.User.DID

tests :: IO TestTree
tests =
  testSpec "Fission.User.DID" do
    describe "Structure" do
      itsProp' "serializes/deserializes to the same" \(did :: DID) ->
        decode' (encode did) == Just did
