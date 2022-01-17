module Web.JWT.Signature.Error (Error (..)) where

import           Fission.Prelude

data Error
  = InvalidPublicKey
  | InvalidSignature
  | SignatureDoesNotMatch
  deriving (Show, Eq, Exception)

instance Display Error where
  display = \case
    InvalidPublicKey      -> "Public key in payload improperly formatted"
    InvalidSignature      -> "Signature improperly formatted"
    SignatureDoesNotMatch -> "Signature does not match content"

