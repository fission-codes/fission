module Fission.CLI.WebNative.FileSystem.Auth.Store.Class (MonadStore (..)) where

import           Crypto.Cipher.AES            (AES256)

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.Key.Symmetric.Types  as Symmetric
import           Fission.User.DID.Types

class Monad m => MonadStore m where
  set :: DID -> FilePath -> Symmetric.Key AES256 -> m ()
  get :: DID -> FilePath -> m (Either (NotFound (Symmetric.Key AES256)) (Symmetric.Key AES256))
