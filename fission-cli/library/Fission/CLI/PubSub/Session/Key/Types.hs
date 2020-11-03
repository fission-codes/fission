module Fission.CLI.PubSub.Session.Key.Types (Key (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

newtype Key = Key
  { aesKey :: Symmetric.Key AES256 }
  deriving newtype Eq

instance ToJSON Key where
  toJSON Key {..} = object ["sessionKey" .= aesKey]

instance FromJSON Key where
  parseJSON = withObject "Key" \obj -> do
    aesKey <- obj .: "sessionKey"
    return Key {..}
