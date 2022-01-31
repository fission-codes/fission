{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Web.DID (spec, rsaKey) where

import           Data.Aeson                            as JSON
import qualified Data.Aeson.Types                      as JSON

import           Servant.API

import qualified RIO.ByteString.Lazy                   as Lazy

import           Crypto.Key.Asymmetric                 as Key
import qualified Crypto.Key.Asymmetric.Algorithm.Types as Alg
import qualified Crypto.PubKey.Ed25519                 as Ed25519

import           Test.Web.UCAN.Prelude

import           Web.DID.Oldstyle.Types
import           Web.DID.Types                         as DID
import qualified Web.DID.Verification                  as DID
import qualified Web.UCAN.Signature                    as Signature
import qualified Web.UCAN.Signature.Error              as Signature


spec :: Spec
spec =
  describe "DID" do
    describe "serialisation" do
      describe "RSA2048" do
        it "serializes to a well-known value"
          let
            expected :: Lazy.ByteString
            -- old format: expected = "did:key:z13V3Sog2YaUKhdGCmgx9UZuW1o1ShFJYc6DvGYe7NTt689NoL2RtpVs65Zw899YrTN9WuxdEEDm54YxWuQHQvcKfkZwa8HTgokHxGDPEmNLhvh69zUMEP4zjuARQ3T8bMUumkSLGpxNe1bfQX624ef45GhWb3S9HM3gvAJ7Qftm8iqnDQVcxwKHjmkV4hveKMTix4bTRhieVHi1oqU4QCVy4QPWpAAympuCP9dAoJFxSP6TNBLY9vPKLazsg7XcFov6UuLWsEaxJ5SomCpDx181mEgW2qTug5oQbrJwExbD9CMgXHLVDE2QgLoQMmgsrPevX57dH715NXC2uY6vo2mYCzRY4KuDRUsrkuYCkewL8q2oK1BEDVvi3Sg8pbC9QYQ5mMiHf8uxiHxTAmPedv8"
            expected = "did:key:z4MXj1wBzi9jUstyNvmiK5WLRRL4rr9UvzPxhry1CudCLKWLyMbP1WoTwDfttBTpxDKf5hAJEjqNbeYx2EEvrJmSWHAu7TJRPTrE3QodbMfRvRNRDyYvaN1FSQus2ziS1rWXwAi5Gpc16bY3JwjyLCPJLfdRWHZhRXiay5FWEkfoSKy6aftnzAvqNkKBg2AxgzGMinR6d1WiH4w5mEXFtUeZkeo4uwtRTd8rD9BoVaHVkGwJkksDybE23CsBNXiNfbweFVRcwfTMhcQsTsYhUWDcSC6QE3zt9h4Rsrj7XRYdwYSK5bc1qFRsg5HULKBp2uZ1gcayiW2FqHFcMRjBieC4LnSMSD1AZB1WUncVRbPpVkn1UGhCU"
          in
            encode (DID.Key rsaKey) `shouldBe` "\"" <> expected <> "\""

      describe "Ed25519" do
        it "serializes to a well-known value"
          let
            expected :: Text
            expected = "did:key:z6MkgYGF3thn8k1Fv4p4dWXKtsXCnLH7q9yw4QgNPULDmDKB"
          in
            encode (DID.Key edKey) `shouldBe` JSON.encode expected

        itsProp' "deserialize . serialize ~ id" \(ed25519pk :: Ed25519.PublicKey) ->
          decode (encode . DID.Key $ Ed25519PublicKey ed25519pk) `shouldBe`
            Just (DID.Key $ Ed25519PublicKey ed25519pk)

        itsProp' "lengths is always 56" \(ed25519pk :: Ed25519.PublicKey) ->
          Lazy.length (encode . DID.Key $ Ed25519PublicKey ed25519pk) `shouldBe` 56 + 2 -- extra 2 for quotes because JSON

        itsProp' "always starts with 'did:key:z6Mk'" \(ed25519pk :: Ed25519.PublicKey) ->
          Lazy.take 13 (encode . DID.Key $ Ed25519PublicKey ed25519pk) `shouldBe` "\"did:key:z6Mk"

        describe "Legacy (AKA `Oldstyle`)" do
          it "deserializes to a well-known value" $
            eitherDecodeStrict ("\"" <> encodeUtf8 oldstyleEdKey <> "\"")
              `shouldBe` Right (DID.Key edKey)

          it "can be manually set to display in the Oldstyle format" $
            textDisplay Oldstyle { did = DID.Key edKey } `shouldBe` oldstyleEdKey

      itsProp' "is a base58 encoded Key DID" \(did :: DID) ->
        Lazy.isPrefixOf "\"did:key:z" (JSON.encode did)

      itsProp' "seralized is isomorphic to ADT" \(did :: DID) ->
        JSON.eitherDecode (JSON.encode did) `shouldBe` Right did

    describe "fixtures" do

      describe "Ed25519" do

        it "verifies alice's signature" do
          runSignatureTest Alg.Ed25519
            aliceDIDEncoded
            helloWorld
            aliceHelloWorldSignature
            `shouldBe`
            Success (Right True)

        it "verifies bob's signature" do
          runSignatureTest Alg.Ed25519
            bobDIDEncoded
            helloWorld
            bobHelloWorldSignature
            `shouldBe`
            Success (Right True)

        it "rejects alice imitating bob" do
          runSignatureTest Alg.Ed25519
            bobDIDEncoded
            helloWorld
            aliceHelloWorldSignature
            `shouldBe`
            Success (Right False)

        it "rejects changed signed data" do
          runSignatureTest Alg.Ed25519
            aliceDIDEncoded
            somethingElse
            aliceHelloWorldSignature
            `shouldBe`
            Success (Right False)

        describe "W3C did:key test vectors" do
          testVectorsW3CEdKey & foldMapM \(idx, bs) ->
            it ("Deserializes vector #" <> show idx <> " to a valid DID") $
              eitherDecode (encode bs) `shouldSatisfy` isEd25519DidKey

      describe "RSA" do
        it "parses a ts-ucan-generated RSA DID" $
          JSON.fromJSON @DID (JSON.String rsaDIDEncoded) `shouldSatisfy` \case
            Success _ -> True
            Error _   -> False

        describe "W3C did:key test vectors" do
          testVectorsW3CRSA & foldMapM \(idx, bs) ->
            it ("Deserializes vector #" <> show idx <> " to a valid DID") $
              eitherDecode (encode bs) `shouldSatisfy` isRSADidKey


runSignatureTest :: Alg.Algorithm -> Text -> ByteString -> Text -> JSON.Result (Either Signature.Error Bool)
runSignatureTest alg didEncoded signedData sigEncoded =
  fmap
    (\(did, signature) -> DID.verifySignature did signedData signature)
    (parseDIDAndSignature alg didEncoded sigEncoded)


parseDIDAndSignature :: Alg.Algorithm -> Text -> Text -> JSON.Result (DID, Signature.Signature)
parseDIDAndSignature alg didEncoded signatureEncoded = do
  did <- JSON.fromJSON @DID $ JSON.String didEncoded
  sig <- JSON.parse (Signature.parse alg) $ JSON.String signatureEncoded
  return (did, sig)


helloWorld :: ByteString
helloWorld = "Hello, World!"

somethingElse :: ByteString
somethingElse = "Something else"

aliceDIDEncoded :: Text
aliceDIDEncoded = "did:key:z6Mkk89bC3JrVqKie71YEcc5M1SMVxuCgNx6zLZ8SYJsxALi"

aliceHelloWorldSignature :: Text
aliceHelloWorldSignature = "tzgopgnzTfvRbaOP+COtqvQJVOfNSKHjlwXz9iMtY/fSWutcCqoeIZqNZwQygu+ntMhsVAst/Rte6I6vAv80AA"

bobDIDEncoded :: Text
bobDIDEncoded = "did:key:z6MkffDZCkCTWreg8868fG1FGFogcJj5X6PY93pPcWDn9bob"

bobHelloWorldSignature :: Text
bobHelloWorldSignature = "PV9pjuzha64gVXCUHghTN3aa6r1lKFOdmH4OJPshDBEgqATA06jWojW377RQQClOyWqRmaNkjQ93UyJvteP9Cg"

rsaDIDEncoded :: Text
rsaDIDEncoded = "did:key:z4MXj1wBzi9jUstyNvmiK5WLRRL4rr9UvzPxhry1CudCLKWLyMbP1WoTwDfttBTpxDKf5hAJEjqNbeYx2EEvrJmSWHAu7TJRPTrE3QodbMfRvRNRDyYvaN1FSQus2ziS1rWXwAi5Gpc16bY3JwjyLCPJLfdRWHZhRXiay5FWEkfoSKy6aftnzAvqNkKBg2AxgzGMinR6d1WiH4w5mEXFtUeZkeo4uwtRTd8rD9BoVaHVkGwJkksDybE23CsBNXiNfbweFVRcwfTMhcQsTsYhUWDcSC6QE3zt9h4Rsrj7XRYdwYSK5bc1qFRsg5HULKBp2uZ1gcayiW2FqHFcMRjBieC4LnSMSD1AZB1WUncVRbPpVkn1UGhCU"


rsaKey :: Key.Public
Right rsaKey = parseUrlPiece "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnzyis1ZjfNB0bBgKFMSvvkTtwlvBsaJq7S5wA+kzeVOVpVWwkWdVha4s38XM/pa/yr47av7+z3VTmvDRyAHcaT92whREFpLv9cj5lTeJSibyr/Mrm/YtjCZVWgaOYIhwrXwKLqPr/11inWsAkfIytvHWTxZYEcXLgAXFuUuaS3uF9gEiNQwzGTU1v0FqkqTBr4B8nW3HCN47XUu0t8Y0e+lf4s4OxQawWD79J9/5d3Ry0vbV3Am1FtGJiJvOwRsIfVChDpYStTcHTCMqtvWbV6L11BWkpzGXSW4Hv43qa+GSYOD2QU68Mb59oSk2OB+BtOLpJofmbGEGgvmwyCI9MwIDAQAB"

edKey :: Key.Public
Right edKey = parseUrlPiece "Hv+AVRD2WUjUFOsSNbsmrp9fokuwrUnjBcr92f0kxw4="

isEd25519DidKey :: Either String DID -> Bool
isEd25519DidKey = \case
  Right (DID.Key (Ed25519PublicKey _)) -> True
  _                                    -> False

isRSADidKey :: Either String DID -> Bool
isRSADidKey = \case
  Right (DID.Key (RSAPublicKey _)) -> True
  _                                -> False

testVectorsW3CEdKey :: [(Natural, Text)]
testVectorsW3CEdKey =
  [ (0, "did:key:z6MkiTBz1ymuepAQ4HEHYSF1H8quG5GLVVQR3djdX3mDooWp")
  , (1, "did:key:z6MkjchhfUsD6mmvni8mCdXHw216Xrm9bQe2mBH1P5RDjVJG")
  , (2, "did:key:z6MknGc3ocHs3zdPiJbnaaqDi58NGb4pk1Sp9WxWufuXSdxf")
  ]

testVectorsW3CRSA :: [(Natural, Text)]
testVectorsW3CRSA =
  [ (0, "did:key:z4MXj1wBzi9jUstyPMS4jQqB6KdJaiatPkAtVtGc6bQEQEEsKTic4G7Rou3iBf9vPmT5dbkm9qsZsuVNjq8HCuW1w24nhBFGkRE4cd2Uf2tfrB3N7h4mnyPp1BF3ZttHTYv3DLUPi1zMdkULiow3M1GfXkoC6DoxDUm1jmN6GBj22SjVsr6dxezRVQc7aj9TxE7JLbMH1wh5X3kA58H3DFW8rnYMakFGbca5CB2Jf6CnGQZmL7o5uJAdTwXfy2iiiyPxXEGerMhHwhjTA1mKYobyk2CpeEcmvynADfNZ5MBvcCS7m3XkFCMNUYBS9NQ3fze6vMSUPsNa6GVYmKx2x6JrdEjCk3qRMMmyjnjCMfR4pXbRMZa3i" )
  , (1, "did:key:zgghBUVkqmWS8e1ioRVp2WN9Vw6x4NvnE9PGAyQsPqM3fnfPf8EdauiRVfBTcVDyzhqM5FFC7ekAvuV1cJHawtfgB9wDcru1hPDobk3hqyedijhgWmsYfJCmodkiiFnjNWATE7PvqTyoCjcmrc8yMRXmFPnoASyT5beUd4YZxTE9VfgmavcPy3BSouNmASMQ8xUXeiRwjb7xBaVTiDRjkmyPD7NYZdXuS93gFhyDFr5b3XLg7Rfj9nHEqtHDa7NmAX7iwDAbMUFEfiDEf9hrqZmpAYJracAjTTR8Cvn6mnDXMLwayNG8dcsXFodxok2qksYF4D8ffUxMRmyyQVQhhhmdSi4YaMPqTnC1J6HTG9Yfb98yGSVaWi4TApUhLXFow2ZvB6vqckCNhjCRL2R4MDUSk71qzxWHgezKyDeyThJgdxydrn1osqH94oSeA346eipkJvKqYREXBKwgB5VL6WF4qAK6sVZxJp2dQBfCPVZ4EbsBQaJXaVK7cNcWG8tZBFWZ79gG9Cu6C4u8yjBS8Ux6dCcJPUTLtixQu4z2n5dCsVSNdnP1EEs8ZerZo5pBgc68w4Yuf9KL3xVxPnAB1nRCBfs9cMU6oL1EdyHbqrTfnjE8HpY164akBqe92LFVsk8RusaGsVPrMekT8emTq5y8v8CabuZg5rDs3f9NPEtogjyx49wiub1FecM5B7QqEcZSYiKHgF4mfkteT2" )
  ]

oldstyleEdKey :: Text
oldstyleEdKey = "did:key:zStEZpzSMtTt9k2vszgvCwF4fLQQSyA15W5AQ4z3AR6Bx4eFJ5crJFbuGxKmbma4"
