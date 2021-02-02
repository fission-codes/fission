module Fission.CLI.WebNative.FileSystem.Auth.Store.Types where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

newtype Store = Store { getStore :: DID :=> (FilePath :=> Symmetric.Key AES256) }
  deriving (Show, Eq, ToJSON, FromJSON)

type k :=> v = Map k v
