-- | 

module Fission.Web.Auth.JWT.Signature.Types (Signature (..)) where

import           Fission.Prelude

data Signature
  = Ed25519 ByteString
  | RS256   ByteString
  deriving (Eq, Show)

instance ToJSON Signature where
  toJSON = String . decodeUtf8Lenient . \case
    Ed25519 bs -> bs
    RS256   bs -> bs
