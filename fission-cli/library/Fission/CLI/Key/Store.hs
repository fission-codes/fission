module Fission.CLI.Key.Store
  ( create
  , forceCreate
  , delete
  , persist
  , exists
  , getAsBytes

  -- * Reexport

  , module Fission.Key.Error
  ) where

import           RIO.Directory
import           RIO.File

import qualified Crypto.PubKey.Curve25519         as Curve25519
import           Crypto.Random

import qualified Data.ByteArray                   as ByteArray

import           Fission.Prelude

import           Fission.CLI.Environment
import           Fission.CLI.Environment.Path     as Path
import           Fission.Key.Error                as Key

import qualified Fission.Internal.Base64          as B64
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

-- Reexports

import           Fission.Key.Error

create ::
  ( MonadIO          m
  , MonadRandom      m
  , MonadEnvironment m
  , MonadRaise       m
  , m `Raises` Key.Error
  )
  => m ()
create = exists >>= \case
  True  -> raise Key.AlreadyExists
  False -> forceCreate

forceCreate ::
  ( MonadIO          m
  , MonadRandom      m
  , MonadEnvironment m
  )
  => m ()
forceCreate = persist =<< Curve25519.generateSecretKey

delete ::
  ( MonadIO          m
  , MonadEnvironment m
  )
  => m ()
delete = exists >>= \case
  False -> return ()
  True  -> removeFile =<< Path.getSigningKeyPath

persist ::
  ( MonadIO          m
  , MonadEnvironment m
  , ByteArray.ByteArrayAccess a
  )
  => a -- Curve25519.SecretKey
  -> m ()
persist key = do
  path <- Path.getSigningKeyPath
  writeBinaryFile path $ B64.toByteString key

getAsBytes ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadRaise       m
  , m `Raises` Key.Error
  )
  => m ByteArray.ScrubbedBytes
getAsBytes =
  exists >>= \case
    False ->
      raise Key.DoesNotExist

    True -> do
      path <- Path.getSigningKeyPath
      bs   <- readFileBinary path
      return $ B64.Scrubbed.scrub bs

exists ::
  ( MonadIO          m
  , MonadEnvironment m
  )
  => m Bool
exists = doesFileExist =<< Path.getSigningKeyPath
