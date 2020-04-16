module Fission.Web.Auth.Token.JWT.Signature.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

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

instance ToServerError Error where
  toServerError err = err422 { errBody = displayLazyBS err }
