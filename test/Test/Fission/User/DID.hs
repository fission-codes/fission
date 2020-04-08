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
              "did:key:z1MdJPaWBebKxtE33AszRWYTF67wCLeFdcsqc3R87hyLKzBKiz49Nyah7i9hzSqMKbQ42UgbfdVFBTRckZECVjkaHTv3QPDKWn1sGRm5GEyzarr4EAT1gUUfXVrwe7satzr3WxZrcpvLzZrviEtV1GhYCr49nyJTn2uamYvozqALP4KKqnR1mgkpo3c8QyZ9DF9HufhXkucFpv8oD5KQWHP8iGhbmqUAWLvTh9CKVx2c2dZWC7cN8VYGWrJYnREUb9t1VptPH15bgVJVVvp1Ho2pervHe37nxoTEM2Ti9cZRKJyUVHdgCjXrpJD4ytSCCSDvTVHXKQitrQTixJoQzBC6dFVKozNUV7eULx5MJq372LQUkz6XJuHK8GgDw8EVNrcmZRDmLVdJGLZDXz3QVJFFQBxDwH7xpd19zciGSoMNnetcAsASMYTx6xCg8u16KE9X8dey38tcSLwREWjaYP8PmmPvVqzBkSsuKw1tSCb7md9axmTP3sKgfyADAcBgk"
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

