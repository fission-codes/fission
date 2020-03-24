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
  ) where

import           Fission.Prelude
import           RIO.Directory
import           RIO.FilePath
import           RIO.File

import qualified Crypto.PubKey.Curve25519 as X
import qualified Crypto.PubKey.Ed25519 as Ed
import           Crypto.Error

import qualified Fission.Internal.Crypto as Crypto

import qualified Data.ByteArray as BA

import           Fission.Key.Error as Key

create :: MonadIO m => m (Either Key.Error ())
create = exists >>= \case
  True -> return <| Left Key.AlreadyExists
  False -> return . Right =<< forceCreate

forceCreate :: MonadIO m => m ()
forceCreate = do
  privkey <- liftIO X.generateSecretKey
  writeKey privkey

delete :: MonadIO m => m ()
delete = exists >>= \case
  False -> return ()
  True -> removeFile =<< location

writeKey :: MonadIO m => X.SecretKey -> m ()
writeKey key = do
  path <- location
  let bs = Crypto.unpack key
  writeBinaryFile path bs

sign ::
  MonadIO m
  => ByteString
  -> m (Either Key.Error Ed.Signature)
sign bs = readEd >>= \case
  Left err -> return <| Left err
  Right sk -> return <| Right <| signWith sk bs

signWith :: Ed.SecretKey -> ByteString -> Ed.Signature
signWith sk bs =
  Ed.sign sk (Ed.toPublic sk) (Crypto.pack bs)

publicKeyX :: MonadIO m => m (Either Key.Error X.PublicKey)
publicKeyX = pure . fmap X.toPublic =<< readX

publicKeyEd :: MonadIO m => m (Either Key.Error Ed.PublicKey)
publicKeyEd = pure . fmap Ed.toPublic =<< readEd

readX :: MonadIO m => m (Either Key.Error X.SecretKey)
readX = readKey X.secretKey

readEd :: MonadIO m => m (Either Key.Error Ed.SecretKey)
readEd = readKey Ed.secretKey

readKey ::
  MonadIO m
  =>(BA.ScrubbedBytes -> CryptoFailable a)
  -> m (Either Key.Error a)
readKey f = readBytes >>= \case
  Left err -> return <| Left err
  Right bytes -> return <| parseKey f bytes

readBytes :: MonadIO m => m (Either Key.Error BA.ScrubbedBytes)
readBytes = exists >>= \case
  False -> return <| Left Key.DoesNotExist
  True -> do
    path <- location
    bs <- readFileBinary path
    return . Right <| Crypto.pack bs

parseKey :: (BA.ScrubbedBytes -> CryptoFailable a) -> BA.ScrubbedBytes -> Either Key.Error a
parseKey f bytes =
  case f bytes of
    CryptoPassed sk -> Right sk
    CryptoFailed err -> Left <| Key.ParseError err

exists :: MonadIO m => m Bool
exists = doesFileExist =<< location

location :: MonadIO m => m FilePath
location = do
  home <- getHomeDirectory
  return <| home </> ".ssh" </> "fission"
