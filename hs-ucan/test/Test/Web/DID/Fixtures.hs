module Test.Web.DID.Fixtures
  ( aliceDID
  , alicePublic
  , aliceKey
  --
  , bobDID
  , bobPublic
  , bobKey
  --
  , malloryDID
  , malloryPublic
  , malloryKey
  ) where

import qualified Crypto.Error
import           Crypto.Key.Asymmetric.Public.Types
import qualified Crypto.PubKey.Ed25519              as Ed25519
import qualified Data.ByteArray                     as BA
import qualified Data.ByteString.Base64             as BS64
import           Data.Monoid
import qualified RIO.ByteString                     as BS
import qualified RIO.Text                           as Text
import           Test.Web.UCAN.Prelude
import           Web.DID.Types
import qualified Web.DID.Types                      as DID


aliceDID, bobDID, malloryDID :: DID
aliceDID   = DID.Key $ Ed25519PublicKey alicePublic -- parseFixture $ JSON.String "did:key:z6Mkk89bC3JrVqKie71YEcc5M1SMVxuCgNx6zLZ8SYJsxALi"
bobDID     = DID.Key $ Ed25519PublicKey bobPublic -- parseFixture $ JSON.String "did:key:z6MkffDZCkCTWreg8868fG1FGFogcJj5X6PY93pPcWDn9bob"
malloryDID = DID.Key $ Ed25519PublicKey malloryPublic -- parseFixture $ JSON.String "did:key:z6MktafZTREjJkvV5mfJxcLpNBoVPwDLhTuMg9ng7dY4zMAL"


aliceKey,    bobKey,    malloryKey    :: Ed25519.SecretKey
alicePublic, bobPublic, malloryPublic :: Ed25519.PublicKey
(aliceKey,   alicePublic)   = expectFixture $ parseNaClEd25519SecretKeyBase64 "U+bzp2GaFQHso587iSFWPSeCzbSfn/CbNHEz7ilKRZ1UQMmMS7qq4UhTzKn3X9Nj/4xgrwa+UqhMOeo4Ki8JUw=="
(bobKey,     bobPublic)     = expectFixture $ parseNaClEd25519SecretKeyBase64 "G4+QCX1b3a45IzQsQd4gFMMe0UB1UOx9bCsh8uOiKLER69eAvVXvc8P2yc4Iig42Bv7JD2zJxhyFALyTKBHipg=="
(malloryKey, malloryPublic) = expectFixture $ parseNaClEd25519SecretKeyBase64 "LR9AL2MYkMARuvmV3MJV8sKvbSOdBtpggFCW8K62oZDR6UViSXdSV/dDcD8S9xVjS61vh62JITx7qmLgfQUSZQ=="


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
