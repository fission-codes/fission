module Fission.User.DID.ION.Document.Types (ValidPKs (..)) where

import           RIO.NonEmpty

import qualified Crypto.PubKey.Ed25519                        as Ed25519
import           Data.Foldable

import           Fission.Prelude

import qualified Fission.Internal.Base64.URL                  as B64.URL

import           Fission.Internal.Orphanage.Ed25519.PublicKey ()

newtype ValidPKs = ValidPKs { pks :: NonEmpty Ed25519.PublicKey }
  deriving (Eq, Show)

instance FromJSON ValidPKs where
  parseJSON = withObject "ION Response" \obj -> do
    doc                <- obj .: "didDocument"
    keyIDs :: [Text]   <- doc .: "authentication"
    pks    :: [Object] <- doc .: "verificationMethod"

    let
      collectValidationKey keyMeta acc = do
        keyID <- keyMeta .: "id"
        jwk   <- keyMeta .: "publicKeyJwk"

        curve    :: Text              <- jwk .: "crv"
        kty      :: Text              <- jwk .: "kty"
        pkB64url :: Text              <- jwk .: "x"
        pk       :: Ed25519.PublicKey <- parseJSON . toJSON $ B64.URL.decode pkB64url

        case (curve, kty, keyID `elem` keyIDs) of
          ("Ed25519", "OKP", True) -> pure (pk : acc)
          _                        -> pure acc

    validKeys <- foldrM collectValidationKey [] pks

    case nonEmpty validKeys of
      Nothing    -> fail "No valid DID keys"
      Just final -> pure $ ValidPKs final
