module Fission.CLI.WebNative.FileSystem.Auth.Store.Class (MonadStore (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric
import           Fission.User.DID.Types

class Monad m => MonadStore m where
  set            :: DID -> FilePath -> Symmetric.Key AES256 -> m ()
  getAllMatching :: DID -> FilePath -> m (Map FilePath (Symmetric.Key AES256))
