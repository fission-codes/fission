module Fission.Key.Store
  ( create
  , forceCreate
  , delete
  , sign
  , signWith
  , publicKeyX
  , publicKeyEd
  , readX
  , readEd
  , exists
  , location
  -- * Reexport
  , module Fission.Key.Error
  ) where

import           RIO.Directory
import           RIO.File
import           RIO.FilePath

import           Crypto.Error
import qualified Crypto.PubKey.Curve25519         as X
import qualified Crypto.PubKey.Ed25519            as Ed

import qualified Data.ByteArray                   as BA

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
  privkey <- liftIO X.generateSecretKey
  writeKey privkey

delete :: MonadIO m => m ()
delete = exists >>= \case
  False -> return ()
  True  -> removeFile =<< location

writeKey :: MonadIO m => X.SecretKey -> m ()
writeKey key = do
  path <- location
  writeBinaryFile path $ B64.toByteString key

sign ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => ByteString
  -> m Ed.Signature
sign bs = do
  sk <- readEd
  return $ signWith sk bs

signWith :: Ed.SecretKey -> ByteString -> Ed.Signature
signWith sk bs = Ed.sign sk (Ed.toPublic sk) bs

publicKeyX ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m X.PublicKey
publicKeyX = X.toPublic <$> readX

publicKeyEd ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Ed.PublicKey
publicKeyEd = Ed.toPublic <$> readEd

readX ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m X.SecretKey
readX = readKey X.secretKey

readEd ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m Ed.SecretKey
readEd = readKey Ed.secretKey

readKey ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => (BA.ScrubbedBytes -> CryptoFailable a)
  -> m a
readKey f = ensureM $ parseKey f <$> readBytes

readBytes ::
  ( MonadIO    m
  , MonadRaise m
  , Raises     m Key.Error
  )
  => m BA.ScrubbedBytes
readBytes = exists >>= \case
  False ->
    raise Key.DoesNotExist

  True -> do
    path <- location
    bs   <- readFileBinary path
    return $ B64.Scrubbed.scrub bs

parseKey :: (BA.ScrubbedBytes -> CryptoFailable a) -> BA.ScrubbedBytes -> Either Key.Error a
parseKey f bytes =
  case f bytes of
    CryptoPassed sk  -> Right sk
    CryptoFailed err -> Left $ Key.ParseError err

exists :: MonadIO m => m Bool
exists = doesFileExist =<< location

location :: MonadIO m => m FilePath
location = do
  home <- getHomeDirectory
  return $ home </> ".ssh" </> "fission"
