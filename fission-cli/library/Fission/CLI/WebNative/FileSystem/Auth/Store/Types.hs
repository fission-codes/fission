module Fission.CLI.WebNative.FileSystem.Auth.Store.Types (Store (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric
import           Fission.User.DID.Types

newtype Store = Store { getStore :: Map DID (Map FilePath (Symmetric.Key AES256)) }
  deriving newtype (Eq, ToJSON, FromJSON)
