module Fission.Web.Auth.JWT.Signature.Types (Signature (..)) where

import qualified Crypto.PubKey.Ed25519 as Ed25519

import           Data.ByteArray

import           Fission.Prelude hiding (length)

import qualified Fission.Web.Auth.JWT.Signature.RS256.Types as RS256
import           Fission.Internal.Orphanage.Ed25519.Signature ()

data Signature
  = Ed25519 Ed25519.Signature
  | RS256   RS256.Signature
  deriving (Eq, Show)

instance ToJSON Signature where
  toJSON = \case
    Ed25519 sig -> toJSON sig
    RS256   bs  -> toJSON  bs

instance ByteArrayAccess Signature where
  length = \case
    Ed25519 ed -> length ed
    RS256   bs -> length bs

  withByteArray = \case
    Ed25519 ed -> withByteArray ed
    RS256   bs -> withByteArray bs
