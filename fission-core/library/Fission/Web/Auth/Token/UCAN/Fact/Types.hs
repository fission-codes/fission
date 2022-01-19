module Fission.Web.Auth.Token.UCAN.Fact.Types (Fact (..)) where

import           Crypto.Cipher.AES           (AES256)
import qualified RIO.Text                    as Text

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

data Fact
  = SessionKey (Symmetric.Key AES256)
  | Unknown Text
  deriving Eq

instance Show Fact where
  show = Text.unpack . textDisplay

instance Display Fact where
  textDisplay = \case
    Unknown    txt -> "Unknown "    <> txt
    SessionKey aes -> "SessionKey " <> textDisplay aes

instance Arbitrary Fact where
  arbitrary = oneof [ Unknown    <$> arbitrary
                    , SessionKey <$> arbitrary
                    ]

instance ToJSON Fact where
  toJSON = \case
    SessionKey key -> object ["sessionKey" .= key]
    Unknown    txt -> String txt

instance FromJSON Fact where
  parseJSON jsn =
    parseSessionKey jsn <|> parseUnknown jsn <|> fail "Unable to parse UCAN.Fact"
    where
      parseUnknown = withText "UCAN.Fact.Unknown" \txt -> return $ Unknown txt

      parseSessionKey = withObject "UCAN.Fact SessionKey" \obj -> do
        rawKey <- obj .: "sessionKey"
        key    <- parseJSON $ String rawKey
        return $ SessionKey key
