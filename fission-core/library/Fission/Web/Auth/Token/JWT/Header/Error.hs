module Fission.Web.Auth.Token.JWT.Header.Error (Error (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error.Class

data Error
  = UnsupportedVersion
  | UnsupportedAlgorithm
  deriving (Show, Eq, Exception)

instance Display Error where
  display UnsupportedAlgorithm = "Unsupported JWT signing algorithm"
  display UnsupportedVersion   = "Unsupported UCAN version"

instance ToServerError Error where
  toServerError = \case
    UnsupportedAlgorithm -> err422 { errBody = displayLazyBS UnsupportedAlgorithm }
    UnsupportedVersion   -> err404 { errBody = displayLazyBS UnsupportedVersion   }
