module Fission.WNFS.Access.Query.Store.Class (Store (..)) where

import           Crypto.Cipher.AES            (AES256)
import           RIO.Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import qualified Fission.Key.Symmetric.Types  as Symmetric
import           Fission.User.Username.Types

class Monad m => Store m where
  insert     :: Username -> FilePath -> Symmetric.Key AES256 -> m ()
  getKeysFor :: Username -> m (Map FilePath (Symmetric.Key AES256))
