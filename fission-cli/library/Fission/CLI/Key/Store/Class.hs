module Fission.CLI.Key.Store.Class (MonadKeyStore (..)) where

import qualified RIO.ByteString.Lazy                          as Lazy
import qualified RIO.Text                                     as Text

import           Data.Binary                                  as Binary
import           Data.ByteArray                               as ByteArray

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                        as Ed25519
import qualified Crypto.PubKey.RSA                            as RSA
import           Crypto.Random.Types

import           Fission.Prelude

import           Fission.CLI.Environment.Class
import           Fission.CLI.Environment.Path

import           Fission.CLI.Key.Store.Types
import           Fission.Key.Error                            as Key

import           Fission.Internal.Orphanage.Ed25519.SecretKey ()
import           Fission.Internal.Orphanage.RSA2048.Private   ()

class
  ( MonadRandom m
  , ByteArrayAccess (SecretKey keyRole)
  )
  => MonadKeyStore keyRole m where
  type SecretKey keyRole
  type PublicKey keyRole

  getPath  :: Proxy keyRole -> m FilePath
  toPublic :: Proxy keyRole -> SecretKey keyRole -> m (PublicKey keyRole)
  generate :: Proxy keyRole -> m (SecretKey keyRole)
  parse    :: Proxy keyRole -> ScrubbedBytes -> m (Either Key.Error (SecretKey keyRole))

instance
  ( MonadRandom m
  , MonadIO m
  , MonadEnvironment m
  )
  => MonadKeyStore SigningKey m where
    type SecretKey SigningKey = Ed25519.SecretKey
    type PublicKey SigningKey = Ed25519.PublicKey

    getPath  _pxy = getSigningKeyPath
    toPublic _pxy = pure . Ed25519.toPublic
    generate _pxy = Ed25519.generateSecretKey
    parse _pxy bs =
      return case Ed25519.secretKey bs of
        CryptoPassed sk  -> Right sk
        CryptoFailed err -> Left . Key.ParseError . Text.pack $ show err

instance
  ( MonadRandom m
  , MonadIO m
  , MonadEnvironment m
  )
  => MonadKeyStore ExchangeKey m where
    type SecretKey ExchangeKey = RSA.PrivateKey
    type PublicKey ExchangeKey = RSA.PublicKey

    getPath  _pxy = getSigningKeyPath
    toPublic _pxy = pure . RSA.private_pub
    generate _pxy = snd <$> RSA.generate 2048 65537
    parse    _pxy scrubbed =
      return case Binary.decodeOrFail (Lazy.pack $ ByteArray.unpack scrubbed) of
        Left  (_, _, msg) -> Left . Key.ParseError $ Text.pack msg
        Right (_, _, key) -> Right key
