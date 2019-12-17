module Fission.Web.Auth.JWT.Error (Error (..)) where

import Fission.Prelude

data Error
  = ParseError
  | BadHeader
  | UnsupportedAlg
  | BadPublicKey
  | BadSignature
  | IncorrectSignature
  | DIDNotSupported
  | Expired
  | TooEarly
  | NoUser
  deriving ( Exception
           , Eq
           , Generic
           , ToJSON
           )

instance Show Error where
  show = \case
    ParseError         -> "Could not parse JWT"
    BadHeader          -> "JWT header improperly formatted"
    UnsupportedAlg     -> "Unsupported JWT signing algorithm"
    BadPublicKey       -> "Public key in payload improperly formatted"
    BadSignature       -> "JWT signature improperly formatted"
    IncorrectSignature -> "JWT signature does not match content"
    DIDNotSupported    -> "DID format not currently supported"
    Expired            -> "JWT is expired"
    TooEarly           -> "JWT used before nbf (not before) time"
    NoUser             -> "DID does not match a Fission user"
