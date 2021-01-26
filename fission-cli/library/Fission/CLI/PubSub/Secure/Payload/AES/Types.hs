module Fission.CLI.PubSub.Secure.Payload.AES.Types (Payload (..)) where

import           Data.ByteArray                           as ByteArray
import qualified Data.ByteString.Base64                   as Base64
import qualified RIO.ByteString.Lazy                      as Lazy
import qualified RIO.Text                                 as Text

import           Crypto.Cipher.AES                        (AES256)
import           Crypto.Cipher.Types

import           Fission.Prelude

import           Fission.Key.EncryptedWith                as EncryptedWith
import qualified Fission.Key.Symmetric.Types              as Symmetric

import           Fission.CLI.PubSub.Secure.Payload.Family

data Payload msg = Payload
  { iv            :: IV AES256
  , secretMessage :: msg `EncryptedWith` Symmetric.Key AES256
  }
  deriving Eq

type instance SecurePayload (Symmetric.Key AES256) expected = Payload expected

instance Display (Payload expected) where
  textDisplay Payload {..} =
    mconcat
      [ "SessionPayload{"
      ,   "secretMessage=" <> textDisplay secretMessage
      ,   ", iv=" <> (decodeUtf8Lenient $ ByteArray.convert iv)
      , "}"
      ]

instance Show (Payload expected) where
  show = Text.unpack . textDisplay

instance ToJSON (Payload msg) where
  toJSON Payload {..} =
    object [ "iv"  .= encodedIV
           , "msg" .= encodedMsg
           ]
    where
      encodedIV  = decodeUtf8Lenient . Base64.encode $ ByteArray.convert iv
      encodedMsg = decodeUtf8Lenient . Base64.encode . Lazy.toStrict $ cipherLBS secretMessage

instance FromJSON (Payload expected) where
  parseJSON = withObject "SessionPayload" \obj -> do
    encMsg :: expected `EncryptedWith` Symmetric.Key AES256 <- obj .: "msg"
    ivTxt <- obj .: "iv"

    let secretMessage = EncryptedWith.fromBase64 encMsg

    case makeIV . Base64.decodeLenient $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return Payload {..}
