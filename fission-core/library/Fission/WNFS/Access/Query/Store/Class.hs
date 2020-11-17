module Fission.WNFS.Access.Query.Store.Class (Store (..)) where

import           Crypto.Cipher.AES            (AES256)

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import qualified Fission.Key.Symmetric.Types  as Symmetric
import           Fission.User.Username.Types

class Monad m => Store m where
  lookup ::
       Username
    -> FilePath
    -> m (Either (NotFound (Symmetric.Key AES256)) (Symmetric.Key AES256))

  insert :: Username -> FilePath -> Symmetric.Key AES256 -> m ()
