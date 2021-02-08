module Fission.CLI.WebNative.Mutation.Auth.Store.Class (MonadStore (..)) where

import           Crypto.Cipher.AES                   (AES256)
import           RIO.Set

import           Fission.Prelude

import qualified Fission.Key.Symmetric.Types         as Symmetric
import qualified Fission.Web.Auth.Token.Bearer.Types as Bearer

class Monad m => MonadStore m where
  insert :: Bearer.Token -> m ()
  getAll :: m (Set Bearer.Token)
