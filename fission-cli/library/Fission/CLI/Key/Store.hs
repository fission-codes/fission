module Fission.CLI.Key.Store
  ( create
  , forceCreate
  , delete
  , writeKey -- FIXME persist key 25519
  , exists
  , readKey -- FIXME naming
  , readBytes --FIXME naminmg
  , readCurve25519
  , readEd25519
  , publicKeyCurve25519
  , publicKeyEd25519
  , sign
  -- * Reexport
  , module Fission.Key.Error
  ) where

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Crypto.Error
import qualified Crypto.PubKey.Curve25519         as Curve25519
import qualified Crypto.PubKey.Ed25519            as Ed25518

import qualified Data.ByteArray                   as ByteArray

import           Fission.Key.Error                as Key
import           Fission.Prelude

import qualified Fission.Internal.Base64          as B64
import qualified Fission.Internal.Base64.Scrubbed as B64.Scrubbed

-- Reexports

import           Fission.Key.Error

create ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m ()
create = exists >>= \case
  True  -> raise Key.AlreadyExists
  False -> forceCreate

forceCreate :: MonadIO m => m ()
forceCreate = do
  privkey <- liftIO Curve25519.generateSecretKey
  writeKey privkey

delete :: MonadIO m => m ()
delete = exists >>= \case
  False -> return ()
  True  -> removeFile =<< signingKeyPath

writeKey :: MonadIO m => Curve25519.SecretKey -> m ()
writeKey key = do
  path <- location
  writeBinaryFile path $ B64.toByteString key

exists :: MonadIO m => m Bool
exists = doesFileExist =<< signingKeyPath

readKey :: -- FIXME read global key
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => (ByteArray.ScrubbedBytes -> CryptoFailable a)
  -> m a
readKey f = ensureM $ parseKey f <$> readBytes

readBytes ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m ByteArray.ScrubbedBytes
readBytes =
  exists >>= \case
    False ->
      raise Key.DoesNotExist

    True -> do
      path <- signingKeyPath
      bs   <- readFileBinary path
      return $ B64.Scrubbed.scrub bs
