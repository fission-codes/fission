module Fission.CLI.WebNative.FileSystem.Auth.Store
  ( create
  , getLeastPrivileged
  , getMostPrivileged
  -- * Reexports
  , module Fission.CLI.WebNative.FileSystem.Auth.Store.Class
  , module Fission.CLI.WebNative.FileSystem.Auth.Store.Types
  ) where

import           Crypto.Cipher.AES                                 (AES256)
import           Crypto.Random.Types
import qualified RIO.Map                                           as Map

import           Fission.Prelude

import           Fission.Error.NotFound.Types
import qualified Fission.Key.Symmetric                             as Symmetric
import           Fission.User.DID.Types

import qualified Fission.CLI.WebNative.FileSystem.Auth.Store.Class as WNFS.Auth
import           Fission.CLI.WebNative.FileSystem.Auth.Store.Types

-- Reexport

import           Fission.CLI.WebNative.FileSystem.Auth.Store.Class

create ::
  ( MonadRandom m
  , MonadStore  m
  )
  => DID
  -> FilePath
  -> m (Symmetric.Key AES256)
create did path = do
  key <- Symmetric.genAES256
  WNFS.Auth.set did path key
  return key

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
