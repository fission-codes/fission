-- | JOSE @"typ"@ (Type) Header Parameter

module Fission.Web.Auth.JWT.Header.Typ.Types (Typ (..)) where

import qualified RIO.Text as Text
import           Fission.Prelude

data Typ
  = JWT
  deriving (Eq, Show, Read)

instance FromJSON Typ where
  parseJSON = withText "JWT.Typ" \case
    "JWT" -> return JWT
    other -> fail (Text.unpack other <> " is not an acceptable JWT typ")

instance ToJSON Typ where
  toJSON JWT = String "JWT"
