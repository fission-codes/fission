module Fission.CLI.Key.Store
  ( create
  , forceCreate
  , delete
  , persist
  , exists
  , getAsBytes
  , fetch

  -- * Reexport

  , module Fission.Key.Error
  , module Fission.CLI.Key.Store.Class
  , module Fission.CLI.Key.Store.Types
  ) where

import           RIO.Directory

import qualified Data.ByteArray                   as ByteArray

import           Fission.Prelude

import           Fission.CLI.File

import           Fission.CLI.Key.Store.Class      as KeyStore
import           Fission.Key.Error                as Key

import qualified Fission.Internal.Base64          as B64
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

-- Reexports

import           Fission.Key.Error

import           Fission.CLI.Key.Store.Class
import           Fission.CLI.Key.Store.Types

create ::
  ( MonadIO       m
  , MonadLogger   m
  , MonadKeyStore m key
  , MonadRaise    m
  , m `Raises` Key.Error
  )
  => Proxy key
  -> m ()
create keyPxy = exists keyPxy >>= \case
  True  -> raise Key.AlreadyExists
  False -> forceCreate keyPxy

forceCreate ::
  ( MonadIO       m
  , MonadLogger   m
  , MonadKeyStore m key
  )
  => Proxy key
  -> m ()
forceCreate pxy = persist pxy =<< KeyStore.generate pxy

persist ::
  ( MonadIO       m
  , MonadLogger   m
  , MonadKeyStore m key
  )
  => Proxy key
  -> SecretKey key
  -> m ()
persist keyRole key = do
  path <- KeyStore.getPath keyRole
  forceWrite path $ B64.toByteString key

fetch ::
  ( MonadIO       m
  , MonadKeyStore m key
  , MonadRaise    m
  , m `Raises` Key.Error
  )
  => Proxy key
  -> m (SecretKey key)
fetch pxy = do
  scrubbed <- getAsBytes pxy
  ensureM $ parse pxy scrubbed

getAsBytes ::
  ( MonadIO       m
  , MonadKeyStore m key
  , MonadRaise    m
  , m `Raises` Key.Error
  )
  => Proxy key
  -> m ByteArray.ScrubbedBytes
getAsBytes keyPxy =
  exists keyPxy >>= \case
    False ->
      raise Key.DoesNotExist

    True -> do
      path <- KeyStore.getPath keyPxy
      bs   <- readFileBinary path
      return $ B64.Scrubbed.scrub bs

exists ::
  ( MonadIO       m
  , MonadKeyStore m key
  )
  => Proxy key
  -> m Bool
exists pxy = doesFileExist =<< KeyStore.getPath pxy

delete ::
  ( MonadIO       m
  , MonadKeyStore m key
  )
  => Proxy key
  -> m ()
delete pxy = exists pxy >>= \case
  False -> return ()
  True  -> removeFile =<< KeyStore.getPath pxy
