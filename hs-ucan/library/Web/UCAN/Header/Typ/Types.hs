-- | JOSE @"typ"@ (Type) Header Parameter

module Web.UCAN.Header.Typ.Types (Typ (..)) where

import           Data.Aeson
import qualified Data.Aeson.Types as JSON
import           RIO
import qualified RIO.Text         as Text
import           Test.QuickCheck

data Typ
  = JWT
  deriving (Eq, Show, Read)

instance Arbitrary Typ where
  arbitrary = return JWT

instance FromJSON Typ where
  parseJSON = parseTypV_0_3

instance ToJSON Typ where
  toJSON JWT = String "JWT"


parseTypV_0_3 :: Value -> JSON.Parser Typ
parseTypV_0_3 = withText "JWT.Typ" \case
    "JWT" -> return JWT
    other -> fail (Text.unpack other <> " is not an acceptable JWT typ")
