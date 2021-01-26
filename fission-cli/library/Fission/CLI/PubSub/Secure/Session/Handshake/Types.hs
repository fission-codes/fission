module Fission.CLI.PubSub.Secure.Session.Handshake.Types (Handshake (..)) where

import           Data.ByteArray                      as ByteArray
import qualified Data.ByteString.Base64              as Base64
import qualified RIO.ByteString.Lazy                 as Lazy

import qualified RIO.Text                            as Text

import           Crypto.Cipher.AES                   (AES256)
import           Crypto.Cipher.Types
import qualified Crypto.PubKey.RSA                   as RSA

import           Fission.Prelude

import           Fission.Key.EncryptedWith           as EncryptedWith
import qualified Fission.Key.Symmetric.Types         as Symmetric

import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

data Handshake = Handshake
  { iv         :: IV AES256
  , sessionKey :: Symmetric.Key AES256 `EncryptedWith` RSA.PrivateKey
  , msg        :: Bearer.Token         `EncryptedWith` Symmetric.Key AES256
  }
  deriving Eq

instance Display Handshake where
  textDisplay Handshake {..} =
    mconcat
      [ "PubSub.Session.Handshake{"
      ,   "sessionKey=" <> textDisplay sessionKey
      ,   ", msg="      <> textDisplay msg
      ,   ", iv="       <> decodeUtf8Lenient (ByteArray.convert iv)
      , "}"
      ]

instance Show Handshake where
  show = Text.unpack . textDisplay

instance ToJSON Handshake where
  toJSON Handshake {..} =
    object [ "iv"         .= encodedIV
           , "msg"        .= encodedMsg
           , "sessionKey" .= encodedKey
           ]
    where
      encodedIV  = decodeUtf8Lenient . Base64.encode $ ByteArray.convert iv
      encodedKey = decodeUtf8Lenient . Base64.encode . Lazy.toStrict $ cipherLBS sessionKey
      encodedMsg = decodeUtf8Lenient . Base64.encode . Lazy.toStrict $ cipherLBS msg

instance FromJSON Handshake where
  parseJSON = withObject "PubSub.Session.Handshake" \obj -> do
    token' :: Bearer.Token         `EncryptedWith` Symmetric.Key AES256 <- obj .: "msg"
    key'   :: Symmetric.Key AES256 `EncryptedWith` RSA.PrivateKey       <- obj .: "sessionKey"

    ivTxt <- obj .: "iv"

    case makeIV . Base64.decodeLenient $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return Handshake { iv
                                  , sessionKey = EncryptedWith.fromBase64 key'
                                  , msg        = EncryptedWith.fromBase64 token'
                                  }
