module Fission.CLI.Linking where

{-
1. Everyone subscribes to channel
2. Requestor broadcasts public key
3. Open a secure channel
4. Provider authentication over UCAN
5. Confirm requestor PIN
6. Credential delegation
-}

import qualified Data.ByteArray                            as ByteArray

import           Crypto.Cipher.Types
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA                         as Crypto.RSA
import           Crypto.Random.Types

import qualified RIO.ByteString                            as BS

import           Fission.User.DID.Types                    as DID

import qualified Data.ByteString.Lazy.Char8                as BS8
import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Crypto.PubKey.RSA.OAEP                    as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                   as RSA

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Fission.Prelude

import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription
import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Topic                 as IPFS.PubSub

import           Fission.CLI.Key.Store                     as KeyStore

import           Fission.Web.Auth.Token.JWT                as JWT

import           Fission.Key.Asymmetric.Public.Types

import           Fission.IPFS.PubSub.Topic



import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT

import qualified Fission.Web.Auth.Token.Bearer.Types       as Bearer

import qualified Fission.Web.Auth.Token.JWT                as UCAN



import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver

import           Fission.Authorization.ServerDID.Class

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Cipher.Types                       (AEAD (..),
                                                            AEADMode (..),
                                                            BlockCipher (..),
                                                            Cipher (..), IV,
                                                            KeySizeSpecifier (..),
                                                            makeIV, nullIV)
import           Crypto.Error                              (CryptoError (..),
                                                            CryptoFailable (..))

import qualified Crypto.Random.Types                       as CRT


import           Data.ByteArray                            (ByteArray)
import           Data.ByteString                           (ByteString)

import           Fission.CLI.Environment.Class

import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN

import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import qualified Fission.Web.Auth.Token.JWT.Error          as JWT

import           Fission.Authorization.Potency.Types

listenToLinkRequests ::
  ( MonadLogger m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  )
  => DID
  -> m ()
listenToLinkRequests targetDID = do
  -- FIXME If root device, plz check first
  machineSK <- KeyStore.fetch @SigningKey
  machinePK <- KeyStore.toPublic (Proxy @SigningKey) machineSK

  let
    machineDID =
      DID Key (Ed25519PublicKey machinePK)

  -- case machineDID == targetDID of
    -- True ->

  let
    topic :: IPFS.PubSub.Topic
    topic = IPFS.PubSub.Topic ("deviceLinking@" <> textDisplay targetDID)

  reattempt 100 do
    throwawaySK <- KeyStore.generate (Proxy @ExchangeKey)
    throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

    requestorExchangeDID <- IPFS.PubSub.Subscription.withQueue topic \tq -> do
      liftIO . atomically $ readTQueue tq

    sessionKey <- SessionKey <$> genAES256






    -- Send sesison key encrypted with requestorExchangeDID
    -- wait 1000ms -- YIKES would be good to get a "got it"
    -- send UCAN with potency of none and the aes key in facts encrypted with the aes key
    -- listen for requestor DID
    -- ASK USER if they want to allow this
    -- create & send delegated access

    let throwawayDID = DID Key (RSAPublicKey throwawayPK)

    pubSubSendClear topic throwawayPK -- STEP 2, yes out of order is actually correct
    sessionKey <- getAuthenticatedSessionKey targetDID topic throwawaySK -- STEP 1-4
    secureSendPIN topic sessionKey -- STEP 5

    ucan <- listenForFinalUCAN myDID topic sessionKey -- STEP 6
    storeUCAN ucan

listenForRequestorExchangeDID = do


fetchUCANForDID = undefined

-- NOTE MonadSTM from the other branch would be nice here
requestFrom ::
  ( MonadLogger m
  , MonadKeyStore m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO     m
  , MonadTime   m
  , ServerDID   m -- FIXME not sevrer, userDID
  , JWT.Resolver m
  , MonadRandom m
  , MonadRescue m
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Sub.SubscribesTo` SessionPayload
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  )
  => DID
  -> DID
  -> m ()
requestFrom targetDID myDID =
  reattempt 10 do
    throwawaySK <- KeyStore.generate (Proxy @ExchangeKey)
    throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

    let throwawayDID = DID Key (RSAPublicKey throwawayPK)

    pubSubSendClear topic throwawayPK -- STEP 2, yes out of order is actually correct
    sessionKey <- getAuthenticatedSessionKey targetDID topic throwawaySK -- STEP 1-4
    secureSendPIN topic sessionKey -- STEP 5

    ucan <- listenForFinalUCAN myDID topic sessionKey -- STEP 6
    storeUCAN ucan
  where
    topic :: IPFS.PubSub.Topic
    topic = IPFS.PubSub.Topic ("deviceLinking@" <> textDisplay targetDID)

storeUCAN = undefined -- FIXME

listenForFinalUCAN ::
  ( MonadIO      m
  , JWT.Resolver m
  , ServerDID    m -- FIXME
  , MonadTime    m
  , MonadRandom  m
  , MonadLogger  m
  , MonadRaise   m
  , m `Sub.SubscribesTo` SessionPayload
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` JWT.Error
  , m `Raises` CryptoError
  , m `Raises` String
  )
  => DID
  -> Topic
  -> SessionKey
  -> m UCAN.JWT -- FIXME Or the raw bytestirng version? At minimum want to validate internally
listenForFinalUCAN targetDID topic sessionKey =
  IPFS.PubSub.Subscription.withQueue topic \tq -> do
  candidateRaw@(UCAN.RawContent txt) <- readAES256 sessionKey tq -- FIXME rename to popSecureMsg

  candidateUCAN <- ensure . eitherDecodeStrict $ encodeUtf8 txt
  ensureM $ UCAN.check candidateRaw candidateUCAN

 -- FIXME actually I think that this step wil be handled by the ServerID -> UsreDID chang
  UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot candidateUCAN
  if sender == targetDID
    then return candidateUCAN
    else listenForFinalUCAN targetDID topic sessionKey

broadcastDID ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  )
  => Topic
  -> DID
  -> m ()
broadcastDID topic did = do
  pubSubSendClear topic did

getAuthenticatedSessionKey ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadTime   m
  , ServerDID   m -- FIXME not server, user DID
  , JWT.Resolver m
  , MonadRaise  m
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey -- NOTE SubscribesToChannel & SubscribesToSecure
  , m `Sub.SubscribesTo` EncryptedWith AES256
  , m `Sub.SubscribesTo` SessionPayload
  , m `Raises` RSA.Error
  , m `Raises` String
  , m `Raises` CryptoError
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  )
  => DID
  -> Topic
  -> RSA.PrivateKey
  -> m SessionKey
getAuthenticatedSessionKey targetDID topic sk = do
  -- STEP 3
  sessionKey <- IPFS.PubSub.Subscription.withQueue topic $ listenForSessionKey sk

  -- STEP 4
  IPFS.PubSub.Subscription.withQueue topic $ listenForValidProof targetDID sessionKey

  -- Bootstrapped & validated session key
  return sessionKey

-- STEP 3
listenForSessionKey ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` RSA.Error
  , m `Raises` String -- FIXME better error
  )
  => RSA.PrivateKey
  -> TQueue (Sub.Message (EncryptedWith RSA.PrivateKey))
  -> m SessionKey
listenForSessionKey throwawaySK tq = readRSA throwawaySK tq

listenForValidProof ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadTime   m
  , JWT.Resolver m
  , ServerDID m -- FIXME not targeting the *server*, so plz fix in check function
  , MonadRaise  m
  , m `Raises` JWT.Error
  , m `Raises` RSA.Error
  , m `Raises` String -- FIXME better error
  , m `Raises` CryptoError
  , m `Raises` UCAN.Resolver.Error
  )
  => DID
  -> SessionKey
  -> TQueue (Sub.Message SessionPayload)
  -> m UCAN.JWT
listenForValidProof targetDID sessionKey@(SessionKey (SymmetricKey rawKey)) tq = do
  candidateRaw@(UCAN.RawContent txt) <- readAES256 sessionKey tq -- FIXME rename to popSecureMsg
  candidateUCAN <- ensure . eitherDecodeStrict $ encodeUtf8 txt

  case (candidateUCAN |> claims |> potency) == AuthNOnly of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True -> do
      case (candidateUCAN |> claims |> facts) of
        [] ->
          raise "No facts" -- FIXME

        (aesFact : _) -> do
          case cipherInit aesFact of
            CryptoFailed _ ->
              raise "Invalid session key in facts"

            CryptoPassed key ->
              case key == rawKey of
                False ->
                  raise "Sesison key doesn't match! ABORT!"

                True -> do
                  ensureM $ UCAN.check candidateRaw candidateUCAN

                  -- FIXME actually I think that this step wil be handled by the ServerID -> UsreDID chang
                  UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot candidateUCAN
                  if sender == targetDID
                    then return candidateUCAN
                    else raise "InvalidSender" -- FIXME better error than string

-- STEP 5
secureSendPIN ::
  ( MonadIO        m
  , MonadLocalIPFS m
  , MonadLogger    m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , m `Raises` CryptoError
  )
  => Topic
  -> SessionKey
  -> m ()
secureSendPIN topic sessionKey = do
  randomBS <- liftIO $ getRandomBytes 6

  let
    pin :: Text
    pin = Text.takeEnd 6 . Text.pack . mconcat $ show <$> BS.unpack randomBS -- FIXME check mod direction

  pubSubSendSecure topic sessionKey pin

pubSubSendClear ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , ToJSON  msg
  , Display msg
  )
  => Topic
  -> msg
  -> m ()
pubSubSendClear topic msg = do
  logDebug $ "Broadcasting in cleartext over IFPS PubSub: " <> textDisplay msg
  void . ensureM $ IPFS.runLocal ["pubsub", "pub", Text.unpack $ textDisplay topic] (encode msg)

pubSubSendSecure ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  , m `Raises` CryptoError
  , ToJSON a
  )
  => Topic
  -> SessionKey
  -> a
  -> m ()
pubSubSendSecure topic (SessionKey aesKey) msg = do
  encrypted <- toSecureMessage aesKey msg
  pubSubSendClear topic encrypted

toSecureMessage ::
  ( MonadRandom m
  , MonadRaise  m
  , m `Raises` CryptoError
  , ToJSON msg
  )
  => SymmetricKey AES256
  -> msg
  -> m SessionPayload
toSecureMessage aesKey msg = do
  genIV >>= \case
    Nothing ->
      undefined -- FIXME better error

    Just iv -> do
      secretMessage <- ensure $ encrypt aesKey iv msg
      return $ SessionPayload {..}

-- fixme rename as SessionPayload, since the pubsub field it livesin will be "payload"
data SessionPayload = SessionPayload
  { secretMessage :: EncryptedWith AES256 -- FIXMe rename ciphertext
  , iv            :: IV AES256
  }
  deriving Eq

instance Display SessionPayload where
  textDisplay SessionPayload {..} =
    mconcat
      [ "SessionPayload{"
      ,   "secretMessage=" <> textDisplay secretMessage
      ,   "iv=" <> (decodeUtf8Lenient $ ByteArray.convert iv)
      , "}"
      ]

instance Show SessionPayload where
  show = Text.unpack . textDisplay

instance ToJSON SessionPayload where
  toJSON SessionPayload {..} =
    object [ "payload" .= secretMessage -- FIXME in the spec! payload -> secretMessage
           , "iv"      .= (decodeUtf8Lenient $ ByteArray.convert iv)
           ]

instance FromJSON SessionPayload where
  parseJSON = withObject "SessionPayload" \obj -> do
    secretMessage <- obj .: "payload"
    ivTxt         <- obj .: "iv"

    case makeIV $ encodeUtf8 ivTxt of
      Nothing -> fail "Invalid (IV AES256)"
      Just iv -> return SessionPayload {..}

newtype EncryptedWith cipher
  = EncryptedPayload { ciphertext :: ByteString }
  deriving newtype (Eq, Show)

instance Display (EncryptedWith cipher) where
  textDisplay EncryptedPayload {ciphertext} =
    decodeUtf8Lenient ciphertext

instance ToJSON (EncryptedWith cipher) where
  toJSON (EncryptedPayload bs) = String $ decodeUtf8Lenient bs

instance FromJSON (EncryptedWith AES256) where
  parseJSON = withText "EncryptedWith AES256" \txt ->
    return . EncryptedPayload $ encodeUtf8 txt

---

newtype SymmetricKey cipher
  = SymmetricKey { rawKey :: ByteString }
  deriving newtype Eq

instance ToJSON (SymmetricKey AES256) where
  toJSON (SymmetricKey bs) = String $ decodeUtf8Lenient bs

instance FromJSON (SymmetricKey AES256) where
  parseJSON = withText "AES256 SymmetricKey" \txt ->
    return . SymmetricKey $ encodeUtf8 txt

---

newtype SessionKey = SessionKey
  { sessionKey :: SymmetricKey AES256 }
  deriving newtype Eq

instance ToJSON SessionKey where
  toJSON SessionKey {..} = object ["sessionKey" .= sessionKey]

instance FromJSON SessionKey where
  parseJSON = withObject "SessionKey" \obj -> do
    sessionKey <- obj .: "sessionKey"
    return SessionKey {..}

readRSA ::
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
  -- ^^^^^^^^^^^^^^^^^ FIXME maybe do this step in the queue handler?
  -> m a
readRSA sk tq = do
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

  where
    oaepParams = RSA.OAEP.defaultOAEPParams SHA256

readAES256 ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` CryptoError
  , FromJSON a
  )
  => SessionKey
  -> TQueue (Sub.Message SessionPayload)
  -- ^^^^^^^^^^^^^^^^^ FIXME maybe do this step in the queue handler?
  -> m a
readAES256 sessionKey@(SessionKey aes256) tq = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  Sub.Message
    { payload = SessionPayload
                  { secretMessage = secretMsg@(EncryptedPayload ciphertext)
                  , iv
                  }
    } <- liftIO . atomically $ readTQueue tq

  case decrypt aes256 iv secretMsg of
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

encrypt ::
  ToJSON a
  => SymmetricKey AES256
  -> IV AES256
  -> a
  -> Either CryptoError (EncryptedWith AES256)
encrypt (SymmetricKey rawKey) iv plaintext =
  case cipherInit rawKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (ciphertext, _) = aeadEncrypt blockCipher . Lazy.toStrict $ encode plaintext
          in
            Right $ EncryptedPayload ciphertext

decrypt ::
     SymmetricKey AES256
  -> IV AES256
  -> EncryptedWith AES256
  -> Either CryptoError ByteString
decrypt (SymmetricKey aesKey) iv (EncryptedPayload ciphertext) =
  case cipherInit aesKey of
    CryptoFailed err ->
      Left err

    CryptoPassed (cipher :: AES256) ->
      case aeadInit AEAD_GCM cipher iv of
        CryptoFailed err ->
          Left err

        CryptoPassed blockCipher ->
          let
            (plaintext, _) = aeadDecrypt blockCipher ciphertext
          in
            Right plaintext

-- | Generates a string of bytes (key) of a specific length for a given block cipher
genAES256 :: MonadRandom m => m (SymmetricKey AES256)
genAES256 = SymmetricKey <$> getRandomBytes (blockSize (undefined :: AES256)) -- FIXME or something?

-- | Generate a random initialization vector for a given block cipher
genIV :: MonadRandom m => m (Maybe (IV AES256))
genIV = do
  bytes <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV (bytes :: ByteString)
