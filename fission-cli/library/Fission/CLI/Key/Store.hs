module Fission.CLI.Key.Store
  ( create
  , forceCreate
  , delete
  , persist
  , exists
  , getAsBytes
  , parseSecretKey

  -- * Reexport

  , module Fission.Key.Error
  , module Fission.CLI.Key.Store.Class
  ) where

import           RIO.Directory

import qualified Data.ByteArray                   as ByteArray

import           Fission.Prelude

import           Fission.CLI.Environment
import           Fission.CLI.Environment.Path     as Path
import           Fission.CLI.File

import           Fission.CLI.Key.Store.Class      as KeyStore
import           Fission.Key.Error                as Key

import qualified Fission.Internal.Base64          as B64
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

-- Reexports

import           Fission.CLI.Key.Store.Class
import           Fission.Key.Error

create ::
  ( MonadIO           m
  , MonadKeyStore key m
  , MonadRaise        m
  , m `Raises` Key.Error
  )
  => Proxy key
  -> m ()
create keyPxy = exists keyPxy >>= \case
  True  -> raise Key.AlreadyExists
  False -> forceCreate keyPxy

getAsBytes ::
  ( MonadIO           m
  , MonadEnvironment  m
  , MonadKeyStore key m
  , MonadRaise        m
  , m `Raises` Key.Error
  )
  => Proxy key
  -> m ByteArray.ScrubbedBytes
getAsBytes keyPxy =
  exists keyPxy >>= \case
    False ->
      raise Key.DoesNotExist

    True -> do
      path <- Path.getSigningKeyPath
      bs   <- readFileBinary path
      return $ B64.Scrubbed.scrub bs

parseSecretKey ::
  ( MonadRaise m
  , MonadKeyStore key m
  , Raises     m Key.Error
  )
  => Proxy key
  -> ByteArray.ScrubbedBytes
  -> m (SecretKey key)
parseSecretKey pxy sb = ensureM $ parse pxy sb

exists ::
  ( MonadIO           m
  , MonadKeyStore key m
  )
  => Proxy key
  -> m Bool
exists pxy = doesFileExist =<< KeyStore.getPath pxy

forceCreate ::
  ( MonadIO           m
  , MonadKeyStore key m
  )
  => Proxy key
  -> m ()
forceCreate pxy = persist pxy =<< KeyStore.generate pxy

delete ::
  ( MonadIO           m
  , MonadKeyStore key m
  )
  => Proxy key
  -> m ()
delete pxy = exists pxy >>= \case
  False -> return ()
  True  -> removeFile =<< KeyStore.getPath pxy

persist ::
  ( MonadIO           m
  , MonadLogger       m
  , MonadKeyStore key m
  )
  => Proxy key
  -> SecretKey key
  -> m ()
persist keyRole key = do
  path <- KeyStore.getPath keyRole
  forceWrite path $ B64.toByteString key
