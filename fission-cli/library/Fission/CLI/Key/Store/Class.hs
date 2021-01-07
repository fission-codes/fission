module Fission.CLI.Key.Store.Class (MonadKeyStore (..)) where

import qualified RIO.ByteString.Lazy                          as Lazy
import           RIO.FilePath                                 ((</>))

import           Data.Binary                                  as Binary
import           Data.ByteArray                               as ByteArray

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                        as Ed25519
import qualified Crypto.PubKey.RSA                            as RSA
import           Crypto.Random.Types

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public
import           Fission.Key.Error                            as Key

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Path

import           Fission.CLI.Key.Store.Types

import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import           Fission.Internal.Orphanage.RSA2048.Private   ()

class
  ( MonadRandom m
  , ByteArrayAccess (SecretKey keyRole)
  )
  => MonadKeyStore m keyRole where
  type SecretKey keyRole
  type PublicKey keyRole

  getPath  :: Proxy keyRole -> m FilePath
  toPublic :: Proxy keyRole -> SecretKey keyRole -> m (PublicKey keyRole)
  generate :: Proxy keyRole -> m (SecretKey keyRole)
  parse    :: Proxy keyRole -> ScrubbedBytes -> m (Either Key.Error (SecretKey keyRole))

instance
  ( MonadIO          m
  , MonadLogger      m
  , MonadRandom      m
  , MonadEnvironment m
  )
  => MonadKeyStore m SigningKey where
    type SecretKey SigningKey = Ed25519.SecretKey
    type PublicKey SigningKey = Ed25519.PublicKey

    toPublic _pxy = pure . Ed25519.toPublic

    generate _pxy = do
      logDebug @Text "Generating signing key"
      Ed25519.generateSecretKey

    getPath _pxy = do
      path <- globalKeyDir
      return $ path </> "machine_id.ed25519"

    parse _pxy bs = do
      logDebug @Text "Parsing signing key"
      return case Ed25519.secretKey bs of
        CryptoPassed sk  -> Right sk
        CryptoFailed err -> Left $ Key.ParseError err

instance
  ( MonadIO          m
  , MonadLogger      m
  , MonadRandom      m
  , MonadEnvironment m
  )
  => MonadKeyStore m ExchangeKey where
    type SecretKey ExchangeKey = RSA.PrivateKey
    type PublicKey ExchangeKey = RSA.PublicKey

    toPublic _pxy = pure . RSA.private_pub

    generate _pxy = do
      logDebug @Text "Generating exchange key"
      genRSA2048

    getPath _pxy = do
      path <- globalKeyDir
      return $ path </> "exchange.rsa2048"

    parse _pxy scrubbed = do
      logDebug @Text "Parsing exchange key"
      return case Binary.decodeOrFail (Lazy.pack $ ByteArray.unpack scrubbed) of
        Left  _           -> Left $ Key.ParseError CryptoError_SecretKeyStructureInvalid
        Right (_, _, key) -> Right key

