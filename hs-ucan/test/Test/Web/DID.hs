module Test.Web.DID (spec) where

import           Data.Aeson                            as JSON
import qualified Data.Aeson.Types                      as JSON

import           Web.DID.Types                         as DID

import qualified Crypto.Key.Asymmetric.Algorithm.Types as Alg

import           Test.Prelude
import qualified Web.DID.Verification                  as DID
import qualified Web.UCAN.Signature                    as Signature
import qualified Web.UCAN.Signature.Error              as Signature


spec :: Spec
spec =
  describe "DID" do
    describe "serialisation" do
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
