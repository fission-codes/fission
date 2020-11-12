module Fission.IPFS.PubSub.Subscription.Secure
  ( popMessage
  , popRSAMessage
  , oaepParams -- FIXME move to more sensible module
  ) where

import           Data.ByteArray                                 as ByteArray

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                         as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                        as RSA
import           Crypto.Random.Types

import           Fission.Prelude

import qualified Fission.Key.Symmetric                          as Symmetric
import           Fission.Security.EncryptedWith.Types

import qualified Fission.IPFS.PubSub.Subscription               as Sub
import           Fission.IPFS.PubSub.Subscription.Message.Types

import qualified Fission.IPFS.PubSub.Session.Key.Types          as Session
import qualified Fission.IPFS.PubSub.Session.Payload.Types      as Session

popMessage ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` CryptoError
  , FromJSON msg
  )
  => Session.Key
  -> TQueue (Session.Payload msg)
  -> m msg
popMessage (Session.Key aes256) tq = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  -- FIXME or at caller?
  Session.Payload
    { secretMessage = secretMsg@(EncryptedPayload ciphertext)
    , iv
    } <- liftIO . atomically $ readTQueue tq

  case Symmetric.decrypt aes256 iv secretMsg of
    Left err -> do
      -- FIXME MOVE THIS PART TO the decrypt function, even it that means wrapping in m
      logDebug $ "Unable to decrypt message via AES256: " <> decodeUtf8Lenient ciphertext
      raise err

    Right clearBS ->
      case eitherDecodeStrict clearBS of
        -- FIXME better "can't decode JSON" error
        Left err -> do
          logDebug $ "Unable to decode AES-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
          raise err

        Right payload ->
          return payload

popRSAMessage ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` RSA.Error
  , FromJSON a
  )
  => RSA.PrivateKey
  -> TQueue (Sub.Message (EncryptedWith RSA.PrivateKey))
  -> m a
popRSAMessage sk tq = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  Sub.Message
    { payload = EncryptedPayload secretMsg
    } <- liftIO . atomically $ readTQueue tq

  RSA.OAEP.decryptSafer oaepParams sk secretMsg >>= \case
    Left err -> do
      logDebug $ "Unable to decrypt message via RSA: " <> decodeUtf8Lenient secretMsg
      raise err

    Right clearBS ->
      case eitherDecodeStrict clearBS of
        -- FIXME better "can't decode JSON" error
        Left err -> do
          logDebug $ "Unable to decode RSA-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
          raise err

        Right payload ->
          return payload

oaepParams ::
  ( ByteArray       output
  , ByteArrayAccess seed
  )
  => RSA.OAEP.OAEPParams SHA256 seed output
oaepParams = RSA.OAEP.defaultOAEPParams SHA256
