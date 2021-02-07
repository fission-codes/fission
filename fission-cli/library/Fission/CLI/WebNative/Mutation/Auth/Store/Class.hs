module Fission.CLI.WebNative.Mutation.Auth.Store.Class (MonadStore (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric
import           Fission.Web.Auth.Token.JWT

class Monad m => MonadStore m where
  insert :: JWT -> m ()
  getAll :: m [JWT]
