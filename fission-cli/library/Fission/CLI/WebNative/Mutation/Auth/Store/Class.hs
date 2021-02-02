module Fission.CLI.WebNative.Mutation.Auth.Store.Class (MonadStore (..)) where

import           Crypto.Cipher.AES           (AES256)

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types as Symmetric

class Monad m => MonadStore m where
  insert :: UCAN -> m ()
  getAll :: m [UCAN]
