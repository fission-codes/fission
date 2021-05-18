{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Fission.Test.User.DID (spec) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.Aeson            as JSON

import qualified RIO.ByteString.Lazy   as Lazy
import           Servant.API

import           Fission.Key           as Key
import           Fission.User.DID

import           Fission.Test.Prelude

spec :: Spec
spec =
  describe "Serialization" do
    context "RSA2048" do
      it "serializes to a well-known value"
        let
          expected :: Lazy.ByteString
          expected = "did:key:z13V3Sog2YaUKhdGCmgx9UZuW1o1ShFJYc6DvGYe7NTt689NoL2RtpVs65Zw899YrTN9WuxdEEDm54YxWuQHQvcKfkZwa8HTgokHxGDPEmNLhvh69zUMEP4zjuARQ3T8bMUumkSLGpxNe1bfQX624ef45GhWb3S9HM3gvAJ7Qftm8iqnDQVcxwKHjmkV4hveKMTix4bTRhieVHi1oqU4QCVy4QPWpAAympuCP9dAoJFxSP6TNBLY9vPKLazsg7XcFov6UuLWsEaxJ5SomCpDx181mEgW2qTug5oQbrJwExbD9CMgXHLVDE2QgLoQMmgsrPevX57dH715NXC2uY6vo2mYCzRY4KuDRUsrkuYCkewL8q2oK1BEDVvi3Sg8pbC9QYQ5mMiHf8uxiHxTAmPedv8"
        in
          encode (DID Key rsaKey) `shouldBe` "\"" <> expected <> "\""

    context "Ed25519" do
      it "serializes to a well-known value"
        let
          expected :: Text
          expected = "did:key:z6MkgYGF3thn8k1Fv4p4dWXKtsXCnLH7q9yw4QgNPULDmDKB"
        in
          encode (DID Key edKey) `shouldBe` JSON.encode expected

      itsProp' "deserialize . serialize ~ id" \(ed25519pk :: Ed25519.PublicKey) ->
        decode (encode . DID Key $ Ed25519PublicKey ed25519pk) `shouldBe`
          Just (DID Key $ Ed25519PublicKey ed25519pk)

      itsProp' "lengths is always 56" \(ed25519pk :: Ed25519.PublicKey) ->
        Lazy.length (encode . DID Key $ Ed25519PublicKey ed25519pk) `shouldBe` 56 + 2 -- extra 2 for quotes because JSON

      itsProp' "always starts with 'did:key:z6Mk'" \(ed25519pk :: Ed25519.PublicKey) ->
        Lazy.take 13 (encode . DID Key $ Ed25519PublicKey ed25519pk) `shouldBe` "\"did:key:z6Mk"

      context "Legacy (AKA `Oldstyle`)" do
        it "deserializes to a well-known value"
          let
            input :: ByteString
            input = "did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4"
          in
            eitherDecodeStrict ("\"" <> input <> "\"")
              `shouldBe` Right (DID Key edKey)

         it "can be manually set to display in the Oldstyle format"
           encode (Oldstyle $ DID Key edKey) `shouldBe` "NOT THIS"

      context "W3C did:key Ed25519 test vectors" do
        didKeyTestVectors |> foldMapM \(idx, bs) ->
          it ("Deserializes vector #" <> show idx <> " to a valid DID") $
            eitherDecode (encode bs) `shouldSatisfy` isEd25519DidKey

    itsProp' "serialized is isomorphic to ADT" \(did :: DID) ->
      JSON.decode (JSON.encode did) `shouldBe` Just did

    itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
      Lazy.isPrefixOf "\"did:key:z" (JSON.encode did)

rsaKey :: Key.Public
Right rsaKey = parseUrlPiece "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB"

edKey :: Key.Public
Right edKey = parseUrlPiece "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="

isEd25519DidKey :: Either String DID -> Bool
isEd25519DidKey = \case
  Right (DID Key (Ed25519PublicKey _)) -> True
  _                                    -> False

didKeyTestVectors :: [(Natural, Text)]
didKeyTestVectors =
  [ (0, "did:key:z6MkiTBz1ymuepAQ4HEHYSF1H8quG5GLVVQR3djdX3mDooWp")
  , (1, "did:key:z6MkjchhfUsD6mmvni8mCdXHw216Xrm9bQe2mBH1P5RDjVJG")
  , (2, "did:key:z6MknGc3ocHs3zdPiJbnaaqDi58NGb4pk1Sp9WxWufuXSdxf")
  ]
