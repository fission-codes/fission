module Web.Ucan.Signature.Types (Signature (..)) where

import qualified Crypto.PubKey.Ed25519                         as Ed25519

import           Data.ByteArray

import           Data.Aeson
import           RIO                                           hiding (length)

import           Web.Ucan.Internal.Orphanage.Ed25519.Signature ()
import qualified Web.Ucan.Signature.RS256.Types                as RS256

data Signature
  = Ed25519 Ed25519.Signature
  | RS256   RS256.Signature
  deriving (Eq, Show)

instance Display Signature where
  textDisplay (Ed25519 sig) = textDisplay sig
  textDisplay (RS256   sig) = textDisplay sig

instance ToJSON Signature where
  toJSON = \case
    Ed25519 sig -> toJSON sig
    RS256   sig -> toJSON sig

instance ByteArrayAccess Signature where
  length = \case
    Ed25519 ed -> length ed
    RS256   bs -> length bs

  withByteArray = \case
    Ed25519 ed -> withByteArray ed
    RS256   bs -> withByteArray bs
