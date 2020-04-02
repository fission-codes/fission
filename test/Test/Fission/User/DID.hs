module Test.Fission.User.DID (tests) where

import qualified Data.Aeson          as JSON
import qualified RIO.ByteString.Lazy as Lazy.BS

import           Fission.User.DID
import           Fission.Key as Key
import           Fission.User.DID.Method.Types

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Fission.User.DID" do
    describe "Serialization" do
      it "serializes RSA2048 and Ed25519 differently"
        let
          rsa = DID (Key.Public "12345") RSA2048 Key
          ed  = rsa { algorithm = Ed25519 }
        in
          encode rsa `shouldNotBe` encode ed

      itsProp' "serialize+deserialize is the identity function" \(did :: DID) ->
        JSON.decode' (JSON.encode did) == Just did

      itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
        Lazy.BS.isPrefixOf "\"did:key:z" (JSON.encode did)

