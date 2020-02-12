module Fission.Web.Auth.JWT.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

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

instance Display Error where
  display = \case
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

instance Show Error where
  show = show . textDisplay

instance ToServerError Error where
  toServerError = \case
    ParseError         -> err422 { errBody = displayLazyBS ParseError         }
    BadHeader          -> err422 { errBody = displayLazyBS BadHeader          }
    UnsupportedAlg     -> err422 { errBody = displayLazyBS UnsupportedAlg     }
    BadPublicKey       -> err422 { errBody = displayLazyBS BadPublicKey       }
    BadSignature       -> err422 { errBody = displayLazyBS BadSignature       }
    IncorrectSignature -> err422 { errBody = displayLazyBS IncorrectSignature }
    DIDNotSupported    -> err422 { errBody = displayLazyBS DIDNotSupported    }
    NoUser             -> err404 { errBody = displayLazyBS NoUser             }
    Expired            -> err410 { errBody = displayLazyBS Expired            }
    TooEarly           -> ServerError { errHTTPCode     = 425
                                      , errReasonPhrase = show TooEarly
                                      , errBody         = ""
                                      , errHeaders      = []
                                      }
