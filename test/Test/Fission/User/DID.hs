{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Fission.User.DID (tests) where

import qualified Data.Aeson        as JSON

import qualified RIO.ByteString.Lazy as Lazy
import           Servant.API

import           Fission.User.DID
import           Fission.Key as Key

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Fission.User.DID" do
    describe "Serialization" do
      context "RSA2048" do
        it "serializes to a well-known value"
          let
            expected :: Lazy.ByteString
            expected = "did:key:z12GZctbAybHN746QxWgfwjyJhaJrefTxVNpd52TovWKpi8Fwhm9yT28FrzWq8Fr5jWuhax7J9Njhf961YQk4BeXHiiDJ8epPJbkLszPExjJd1NvmQBDUYrkg6vKmNYvh63xcYCrAiq3p6WX2QCsbK7H6956NTwQVMEdNopEgShkZ5rBiXQBMTnLd5btH2uFF8RtR9oF8bWRxzWywmy6R4VsoVei5wBh1jmJUcvnaUe7paWWpFx5NJzAkwg44ccAdgc7qUvrtS6JsZeGs9Z2YEFGw4r3qSU4gJSm5AdhL9ahiJAxKSVwrqziVDEQEonGGriYrccnwE3TCCDoBWC91bHvt46FSsbS8W1BWjp84VhJvRQ4Ay9Rw1C1X6T7cbpJ8NnP6yp7SDtxAm8DP8ELrWKCqjW8tJHeXEVvwe94hMkbpccr86wsNHPzofx9s1VGvmyfvBjVojGnnMmWK1RuiRZL2yzjo4fYotz7BfLtYCRCC4osozE3YJdBLNYeMjNZq3xBoN5dmYHE8nofQjefP7m1mhVL5"
          in
            encode (DID rsaKey Key) `shouldBe` "\"" <> expected <> "\""

      context "ED25519" do
        it "serializes to a well-known value"
          let
            expected :: Text
            expected = "did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4" -- BR4m3DNZHT1G8Nb2RHzgKK7TrWxEmJjZskgvFeJwYJ6kpzy1PVDvn3jR2vaAWExNdtKT7KzBoAdy8GHeGd8jpiAUDgbRRnMy"
          in
            encode (DID edKey Key) `shouldBe` JSON.encode expected

      -- itsProp' "serialize+deserialize is the identity function" \(did :: DID) ->
      --   JSON.decode (JSON.encode did) == Just did

      itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
        Lazy.isPrefixOf "\"did:key:z" (JSON.encode did)

rsaKey :: Key.Public
Right rsaKey = parseUrlPiece "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB"

edKey :: Key.Public
Right edKey = parseUrlPiece "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="
