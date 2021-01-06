{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Fission.User.DID (tests) where

import qualified Data.Aeson           as JSON

import qualified RIO.ByteString.Lazy  as Lazy
import           Servant.API

import           Fission.Key          as Key
import           Fission.User.DID

import           Test.Fission.Prelude

tests :: IO TestTree
tests =
  testSpec "Fission.User.DID" do
    describe "Serialization" do
      context "RSA2048" do
        it "serializes to a well-known value"
          let
            expected :: Lazy.ByteString
            expected = "did:key:z13V3Sog2YaUKhdGCmgx9UZuW1o1ShFJYc6DvGYe7NTt689NoL2RtpVs65Zw899YrTN9WuxdEEDm54YxWuQHQvcKfkZwa8HTgokHxGDPEmNLhvh69zUMEP4zjuARQ3T8bMUumkSLGpxNe1bfQX624ef45GhWb3S9HM3gvAJ7Qftm8iqnDQVcxwKHjmkV4hveKMTix4bTRhieVHi1oqU4QCVy4QPWpAAympuCP9dAoJFxSP6TNBLY9vPKLazsg7XcFov6UuLWsEaxJ5SomCpDx181mEgW2qTug5oQbrJwExbD9CMgXHLVDE2QgLoQMmgsrPevX57dH715NXC2uY6vo2mYCzRY4KuDRUsrkuYCkewL8q2oK1BEDVvi3Sg8pbC9QYQ5mMiHf8uxiHxTAmPedv8"
          in
            encode (DID Key rsaKey) `shouldBe` "\"" <> expected <> "\""

      context "ED25519" do
        it "serializes to a well-known value"
          let
            expected :: Text
            expected = "did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4"
          in
            encode (DID Key edKey) `shouldBe` JSON.encode expected

      itsProp' "serialized is isomorphic to ADT" \(did :: DID) ->
        JSON.decode (JSON.encode did) == Just did

      itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
        Lazy.isPrefixOf "\"did:key:z" (JSON.encode did)

rsaKey :: Key.Public
Right rsaKey = parseUrlPiece "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB"

edKey :: Key.Public
Right edKey = parseUrlPiece "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="
