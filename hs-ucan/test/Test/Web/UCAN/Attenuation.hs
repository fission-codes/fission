{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Web.UCAN.Attenuation (spec) where

import           Control.Monad.Time
import qualified Crypto.Error
import           Crypto.Key.Asymmetric.Public.Types
import qualified Crypto.PubKey.Ed25519                 as Ed25519
import qualified Data.Aeson.Types                      as JSON
import qualified Data.ByteArray                        as BA
import qualified Data.ByteString.Base64                as BS64
import           Data.Monoid
import qualified RIO.ByteString                        as BS
import qualified RIO.Set                               as Set
import qualified RIO.Text                              as Text
import qualified Text.URI.QQ                           as URI

import           Test.Web.UCAN.Prelude

import           RIO.Time
import qualified Test.Web.UCAN.DelegationSemantics     as DelegationSemantics
import qualified Test.Web.UCAN.Example                 as Ex
import           Test.Web.UCAN.Orphanage.DummyResolver ()
import           Web.DID.Types
import qualified Web.DID.Types                         as DID
import           Web.UCAN.Capabilities
import           Web.UCAN.Types
import           Web.UCAN.Witness


spec :: Spec
spec =
  describe "Attenuation" do
    describe "capabilities" do
      itsPropSized "produces parenthood proofs on UCANs without proofs" 6 \(ucan :: UCAN () Ex.Resource Ex.Ability) -> do
        proofs <- capabilities ucan{ claims = (claims ucan){ proofs = [] } }
        let isParenthoodWitness = \case
              Right (ProofAuthorization _ _ _ (ProofByDelegation _)) -> False
              _                                                      -> True
        all isParenthoodWitness proofs `shouldBe` True

      itsPropSized "produces only valid proofs" 6 \(ucan :: UCAN () Ex.Resource Ex.Ability) -> do
        proofs <- capabilities ucan
        all (either (const True) checkProof) proofs `shouldBe` True

      describe "fixtures" do
        -- TODO Test UCAN spec fixtures.

        it "works with partial-order delgation semantics" do
          tomorrow <- addUTCTime nominalDay <$> currentTime
          let
            leafUcan = signEd25519 @JSON.Value aliceKey Claims
              { sender = aliceDID
              , receiver = bobDID
              , attenuation =
                [ CapResource
                    (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|]])
                    (Ability Ex.MsgSend)
                , CapResource
                    (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|Apps|]])
                    (Ability Ex.MsgSend)
                ]
              , proofs = []
              , facts = []
              , expiration = tomorrow
              , notBefore = Nothing
              , nonce = Nothing
              }

            ucan = signEd25519 @JSON.Value bobKey Claims
              { sender = bobDID
              , receiver = malloryDID
              , attenuation =
                [ CapResource
                    (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|], [URI.pathPiece|file.txt|]])
                    (Ability Ex.MsgSend)
                , CapResource
                    (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|abc|]])
                    (Ability Ex.MsgSend)
                ]
              , proofs = [ Nested $ textDisplay leafUcan ]
              , facts = []
              , expiration = tomorrow
              , notBefore = Nothing
              , nonce = Nothing
              }

            expectedCapabilities =
              [ ( (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|], [URI.pathPiece|file.txt|]], Ability Ex.MsgSend)
                , aliceDID
                )
              , ( (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|], [URI.pathPiece|file.txt|]], Ability Ex.MsgSend)
                , bobDID
                )
              , ( (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|abc|]], Ability Ex.MsgSend)
                , bobDID
                )
              ]

          actualCapabilities <- capsWithOriginators <$> capabilities ucan

          Set.fromList actualCapabilities `shouldBe`
            Set.fromList expectedCapabilities

      describe "PathResource" do
        DelegationSemantics.itHasPartialOrderProperties @Ex.PathResource

        it "public/ can delegate public/test" do
          (Ex.PathResource [[URI.pathPiece|public|]] `canDelegate` Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|]])
            `shouldBe` True

        it "public/ can not delegate private/" do
          (Ex.PathResource [[URI.pathPiece|public|]] `canDelegate` Ex.PathResource [[URI.pathPiece|private|]])
            `shouldBe` False

        it "public/test can not delegate public/" do
          (Ex.PathResource [[URI.pathPiece|public|], [URI.pathPiece|test|]] `canDelegate` Ex.PathResource [[URI.pathPiece|public|]])
            `shouldBe` False



aliceDID, bobDID, malloryDID :: DID
aliceDID   = DID.Key $ Ed25519PublicKey alicePublic -- parseFixture $ JSON.String "did:key:z6Mkk89bC3JrVqKie71YEcc5M1SMVxuCgNx6zLZ8SYJsxALi"
bobDID     = DID.Key $ Ed25519PublicKey bobPublic -- parseFixture $ JSON.String "did:key:z6MkffDZCkCTWreg8868fG1FGFogcJj5X6PY93pPcWDn9bob"
malloryDID = DID.Key $ Ed25519PublicKey malloryPublic -- parseFixture $ JSON.String "did:key:z6MktafZTREjJkvV5mfJxcLpNBoVPwDLhTuMg9ng7dY4zMAL"


aliceKey,    bobKey                   :: Ed25519.SecretKey
alicePublic, bobPublic, malloryPublic :: Ed25519.PublicKey
(aliceKey, alicePublic)   = expectFixture $ parseNaClEd25519SecretKeyBase64 "U+bzp2GaFQHso587iSFWPSeCzbSfn/CbNHEz7ilKRZ1UQMmMS7qq4UhTzKn3X9Nj/4xgrwa+UqhMOeo4Ki8JUw=="
(bobKey,   bobPublic)     = expectFixture $ parseNaClEd25519SecretKeyBase64 "G4+QCX1b3a45IzQsQd4gFMMe0UB1UOx9bCsh8uOiKLER69eAvVXvc8P2yc4Iig42Bv7JD2zJxhyFALyTKBHipg=="
(_,        malloryPublic) = expectFixture $ parseNaClEd25519SecretKeyBase64 "LR9AL2MYkMARuvmV3MJV8sKvbSOdBtpggFCW8K62oZDR6UViSXdSV/dDcD8S9xVjS61vh62JITx7qmLgfQUSZQ=="


expectFixture :: Show e => Either e a -> a
expectFixture = \case
  Left e  -> error $ show e
  Right a -> a


parseNaClEd25519SecretKeyBase64 :: ByteString -> Either Text (Ed25519.SecretKey, Ed25519.PublicKey)
parseNaClEd25519SecretKeyBase64 bs = do
  bytes <- BS64.decodeBase64 bs
  () <- if BS.length bytes == 64 then Right () else Left $ "Unexpected length (" <> textDisplay (BS.length bs) <> " bytes). Expected 64 bytes."
  let (secretKeyBytes, publicKeyBytes) = BS.splitAt 32 bytes
  secretKey <- ensureCryptoPassed $ Ed25519.secretKey secretKeyBytes
  let publicKey = Ed25519.toPublic secretKey
  let actualPublicKeyBytes = BS.pack $ BA.unpack publicKey
  () <- if actualPublicKeyBytes == publicKeyBytes then Right () else Left $ "Secret key didn't match expected public key."
  return (secretKey, publicKey)
  where
    ensureCryptoPassed = \case
      Crypto.Error.CryptoPassed a   -> Right a
      Crypto.Error.CryptoFailed err -> Left $ Text.pack $ show err


capsWithOriginators :: [Either a (Proof fct res abl)] -> [((res, Ability abl), DID)]
capsWithOriginators = concatMap \case
  Right proof@(ProofAuthorization res abl _ _) -> [((res, abl), originator proof)]
  _                                            -> []
