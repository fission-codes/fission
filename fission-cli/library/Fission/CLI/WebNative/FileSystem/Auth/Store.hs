module Fission.CLI.WebNative.FileSystem.Auth.Store
  ( getLeastPrivileged
  , getMostPrivileged
  -- * Reexports
  , module Fission.CLI.WebNative.FileSystem.Auth.Store.Class
  , module Fission.CLI.WebNative.FileSystem.Auth.Store.Types
  ) where

import           Crypto.Cipher.AES                                 (AES256)
import qualified RIO.Map                                           as Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.Key.Symmetric.Types                       as Symmetric
import           Fission.User.DID.Types
import           Fission.Web.Auth.Token.UCAN.Resource.Scope.Types

import           Fission.CLI.WebNative.FileSystem.Auth.Store.Class
import           Fission.CLI.WebNative.FileSystem.Auth.Store.Types

getLeastPrivileged ::
  ( MonadStore m
  , MonadRaise m
  , m `Raises` NotFound (Symmetric.Key AES256)
  )
  => DID
  -> FilePath
  -> m (FilePath, Symmetric.Key AES256)
getLeastPrivileged did path = do
  didStore <- getAllMatching did path
  case Map.lookupMin didStore of
    Nothing -> raise $ NotFound @(Symmetric.Key AES256)
    Just kv -> return kv

getMostPrivileged ::
  ( MonadStore m
  , MonadRaise m
  , m `Raises` NotFound (Symmetric.Key AES256)
  )
  => DID
  -> FilePath
  -> m (FilePath, Symmetric.Key AES256)
getMostPrivileged did path = do
  didStore <- getAllMatching did path
  case Map.lookupMax didStore of
    Nothing -> raise $ NotFound @(Symmetric.Key AES256)
    Just kv -> return kv
