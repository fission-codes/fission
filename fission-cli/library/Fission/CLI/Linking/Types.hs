module Fission.CLI.Linking.Types (LinkData (..)) where

import           Fission.Prelude

data LinkData = LinkData
  { readKey :: Symmetric.Key AES256
  , ucanRaw :: UCAN.RawContent
  , ucanJWT :: UCAN.JWT
  }
  deriving (Eq, Show)

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
