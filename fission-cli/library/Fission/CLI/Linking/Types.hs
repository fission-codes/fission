module Fission.CLI.Linking.Types (LinkData (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric
import qualified Fission.Web.Auth.Token.JWT  as UCAN

data LinkData = LinkData
  { readKey :: Symmetric.Key AES256
  , ucanRaw :: UCAN.RawContent
  , ucanJWT :: UCAN.JWT
  }
  deriving Eq

instance ToJSON LinkData where
  toJSON LinkData {..} =
    object [ "readKey" .= readKey
           , "ucan"    .= ucanRaw
           ]

instance FromJSON LinkData where
  parseJSON = withObject "LinkData" \obj -> do
    readKey <- obj .: "readKey"
    ucanRaw <- obj .: "ucan"
    ucanJWT <- obj .: "ucan" -- Yes, twice
    return LinkData {..}
