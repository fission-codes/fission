module Web.JWT.Header.Error (Error (..)) where

import           Fission.Prelude

data Error
  = UnsupportedVersion
  | UnsupportedAlgorithm
  deriving (Show, Eq, Exception, Generic, NFData)

instance Display Error where
  display UnsupportedAlgorithm = "Unsupported JWT signing algorithm"
  display UnsupportedVersion   = "Unsupported UCAN version"

