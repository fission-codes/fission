module Fission.CLI.Linking.Authorization.Types (Authorization (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric
import qualified Fission.Web.Auth.Token.JWT  as UCAN

data Authorization = Authorization
  { readKey :: Symmetric.Key AES256
  , ucanRaw :: UCAN.RawContent
  , ucanJWT :: UCAN.JWT
  }
  deriving Eq

instance ToJSON Authorization where
  toJSON Authorization {..} =
    object [ "readKey" .= readKey
           , "ucan"    .= ucanRaw
           ]

instance FromJSON Authorization where
  parseJSON = withObject "Authorization" \obj -> do
    readKey <- obj .: "readKey"
    ucanRaw <- obj .: "ucan"
    ucanJWT <- obj .: "ucan" -- NOTE Twice because different formats
    return Authorization {..}
