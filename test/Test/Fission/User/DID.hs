module Test.Fission.User.DID (tests) where

import qualified Data.Aeson          as JSON
import qualified RIO.ByteString.Lazy as Lazy.BS

import           Fission.User.DID
import           Fission.Key as Key

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

      context "RSA2048" do
        it "serializes to a well-known value"
          let
            rsaKey :: Text
            rsaKey = "AAAAB3NzaC1yc2EAAAADAQABAAABAQDkrRwcO9XZOWdwcK9CUQbzD3NMGlmkoRWu/BS5b/C9lm7PIyjBIhshnd6Y29upBKra7dJ7b1qOJDRQS5uvu93OZi/6pGXcqlYHS9WWJtpEQM+VXeJ2PcnKl5ok2mWgeOEqjHRorT+2dVlISjvOk4dRTJR2sB3el8ynQ1W7LuiEio22352O0DYV89DMhMPVVoSvXVBbsvuJv4VJ4e2XYlilsYyF/6zba4rvEP37MJBExNUqlWUbmIAzFbSoJSdickzHJtLCaBu8Eapu/bu90ecNiFIEaXDSvjD+wVqNwqaarWDor248BULN0u3mVTxHh185k8kBAK6ITBnDMJzjsk11"

            expected :: Text
            expected =
              "did:key:z1ArJG77479Lcdo6AYX1efdvVo2dhSeAwPpazVKhFb8Sg4w35AKjx4sdfLy36LnuBq8b3JGt4SJCsnT3mmnujxxehgTiNcXw495k5dafYE9Zt77T8xCWdDkrLLvbGBzV17vNunkgkZKaA5WqwhV4QmFdFCCvEHszNe62SxFCR6g2YqAimX1DSWwVQFc12iuRYrXj8nWeMv2iQG3ZMCvKo2XsW7SXR1Kai29HZdwFQQneEdoEbTWHtFRUh5MpZuX95WNkaZy4JWHjQUNGgNRM6gGrgnmF5ibbXXoTNGhwCH2Qt9DR9khd6t38ufNUdcffapWk5cuGo5pfM1Zye9vapGMpLxQgRUnSyXiz41tSbMHzveTYggiCShVZJ27VLigREDMBqbU8bUNXZBL5L2yTaQieUhPbHsuM1FUD4AGfGQwAmdUstsMC4tjLoYjRBe6vJnwMMxB2ks89M2Yu2aYPf5WgkRGAX9EeXmmiq72A2dPThP4XGaP8pmWw7658BUTuA"
          in
            encode (DID (Key.Public rsaKey) RSA2048 Key)
              `shouldBe` JSON.encode expected

      context "ED25519" do
        it "serializes to a well-known value"
          let
            edKey :: Text
            edKey = "AAAAC3NzaC1lZDI1NTE5AAAAIIPnL+R9+OrIm26I1MSOnu4ofAtJ5PjmfiO9ukShjoST"
 
            expected :: Text
            expected =
              "did:key:zBR4m3DNZHT1G8Nb2RHzgKK7TrWxEmJjZskgvFeJwYJ6kpzy1PVDvn3jR2vaAWExNdtKT7KzBoAdy8GHeGd8jpiAUDgbRRnMy"
          in
            encode (DID (Key.Public edKey) Ed25519 Key)
              `shouldBe` JSON.encode expected

      itsProp' "serialize+deserialize is the identity function" \(did :: DID) ->
        JSON.decode' (JSON.encode did) == Just did

      itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
        Lazy.BS.isPrefixOf "\"did:key:z" (JSON.encode did)

