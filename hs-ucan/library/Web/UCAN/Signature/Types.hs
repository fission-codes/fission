module Web.UCAN.Signature.Types (Signature (..)) where

import qualified Crypto.PubKey.Ed25519                         as Ed25519

import           Data.ByteArray

import           Data.Aeson
import           RIO                                           hiding (length)

import           Web.UCAN.Internal.Orphanage.Ed25519.Signature ()
import qualified Web.UCAN.Signature.RS256.Types                as RS256
import qualified Web.UCAN.Signature.Secp256k1.Types            as Secp256k1

data Signature
  = Ed25519   Ed25519.Signature
  | RS256     RS256.Signature
  | Secp256k1 Secp256k1.Signature
  deriving (Eq, Show)

instance Display Signature where
  textDisplay (Ed25519 sig)   = textDisplay sig
  textDisplay (RS256   sig)   = textDisplay sig
  textDisplay (Secp256k1 sig) = textDisplay sig

instance ToJSON Signature where
  toJSON = \case
    Ed25519   sig -> toJSON sig
    RS256     sig -> toJSON sig
    Secp256k1 sig -> toJSON sig

instance ByteArrayAccess Signature where
  length = \case
    Ed25519 ed   -> length ed
    RS256   bs   -> length bs
    Secp256k1 se -> length se

  withByteArray = \case
    Ed25519 ed   -> withByteArray ed
    RS256   bs   -> withByteArray bs
    Secp256k1 se -> withByteArray se
