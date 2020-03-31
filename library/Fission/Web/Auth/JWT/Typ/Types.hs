-- | 

module Fission.Web.Auth.JWT.Typ.Types (Typ (..)) where

import qualified RIO.Text as Text
import           Fission.Prelude

data Typ
  = JWT
  deriving ( Eq
           , Read
           , Show
           )

instance FromJSON Typ where
  parseJSON = withText "JWT.Typ" \case
    "JWT" -> return JWT
    other -> fail (Text.unpack other <> " is not an acceptable JWT typ")

instance ToJSON Typ where
  toJSON JWT = String "JWT"

-- Secure Self Signed Nested Authenticated Token
