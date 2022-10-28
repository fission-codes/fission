module Fission.Web.Auth.Token.UCAN.Fact.Types (Fact (..)) where

import qualified Data.Aeson                  as JSON
import           Crypto.Cipher.AES           (AES256)
import qualified RIO.Text                    as Text
import           Servant.API

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

data Fact
  = SessionKey (Symmetric.Key AES256)
  | Unknown Text
  | SomeJSON JSON.Object
  deriving Eq

instance Show Fact where
  show = Text.unpack . textDisplay

instance Display Fact where
  textDisplay = \case
    Unknown    txt -> "Unknown "    <> txt
    SessionKey aes -> "SessionKey " <> textDisplay aes
    SomeJSON   obj -> "JSONObj"     <> Text.pack (show obj)

instance Arbitrary Fact where
  arbitrary = oneof [ Unknown    <$> arbitrary
                    , SessionKey <$> arbitrary
                    ]

instance ToJSON Fact where
  toJSON = \case
    SessionKey key -> object ["sessionKey" .= key]
    Unknown    txt -> String txt
    SomeJSON   obj -> Object obj

instance FromJSON Fact where
  parseJSONList = withArray "[]" \vec ->
    sequence $ fmap parseJSON $ toList vec

  parseJSON jsn =
    parseSessionKey jsn <|> parseUnknown jsn <|> parseObj jsn <|> fail "Unable to parse UCAN.Fact"
    where
      parseUnknown =
        withText "UCAN.Fact.Unknown" \txt -> return $ Unknown txt

      parseObj = withObject "UCAN.Fact.UnkownObj" \obj ->
        return $ SomeJSON obj

      parseSessionKey = withObject "UCAN.Fact SessionKey" \obj -> do
        rawKey <- obj .: "sessionKey"
        key    <- parseJSON $ String rawKey
        return $ SessionKey key


instance FromHttpApiData Fact where
  parseUrlPiece txt =
    case parseUrlPiece txt of
      Left _    -> pure $ Unknown txt
      Right key -> pure $ SessionKey key
