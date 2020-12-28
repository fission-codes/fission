module Fission.Key.Symmetric.AES256.Payload.Types (Payload (..)) where

import           Data.ByteArray                       as ByteArray
import qualified Data.ByteString.Base64               as Base64
import qualified RIO.ByteString.Lazy                  as Lazy
import qualified RIO.Text                             as Text

import           Crypto.Cipher.AES                    (AES256)
import           Crypto.Cipher.Types

import           Fission.Prelude

import           Fission.Security.EncryptedWith.Types

data Payload expected = Payload
  { secretMessage :: expected `EncryptedWith` AES256
  , iv            :: IV AES256
  }
  deriving Eq

instance Display (Payload expected) where
  textDisplay Payload {..} =
    mconcat
      [ "SessionPayload{"
      ,   "secretMessage=" <> textDisplay secretMessage
      ,   "iv=" <> (decodeUtf8Lenient $ ByteArray.convert iv)
      , "}"
      ]

instance Show (Payload expected) where
  show = Text.unpack . textDisplay

-- FIXME maybe use a wrapper to base64 encode everything?
instance ToJSON (Payload expected) where
  toJSON Payload {..} =
    object [ "secretMessage" .= (decodeUtf8Lenient . Base64.encode . Lazy.toStrict $ cipherLBS secretMessage)
           , "iv"            .= (decodeUtf8Lenient . Base64.encode $ ByteArray.convert iv)
           ]

instance FromJSON (Payload expected) where
  parseJSON = withObject "SessionPayload" \obj -> do
    secretMessage <- obj .: "secretMessage"
    ivTxt         <- obj .: "iv"

    -- FIXME do we need to base64 decode the IV??? Doesn't seem so
    case makeIV $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return Payload {..}
