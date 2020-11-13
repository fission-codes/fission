module Fission.CLI.Linking.Request
  ( requestFrom
  , getAuthenticatedSessionKey
  , secureSendPIN
  , listenForFinalUCAN
  , listenForSessionKey
  , listenForValidProof
  ) where

import           Data.ByteArray                            as ByteArray

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                    as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types

import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import qualified Network.WebSockets                        as WS

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import           Fission.User.DID.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import qualified Fission.IPFS.PubSub.Session.Key.Types     as Session
import qualified Fission.IPFS.PubSub.Session.Payload       as Session

import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription
import           Fission.IPFS.PubSub.Topic

import           Fission.CLI.Key.Store                     as KeyStore
import qualified Fission.CLI.Linking.PIN                   as PIN

import           Fission.CLI.IPFS.Daemon                   as IPFS.Daemon

import qualified Fission.IPFS.PubSub.Publish               as Publish
import qualified Fission.IPFS.PubSub.Subscription.Secure   as Secure

import           Fission.IPFS.PubSub.Session.Payload       as Payload
import           Fission.Security.EncryptedWith.Types

import           Fission.Web.Auth.Token.Bearer.Types       as Bearer

requestFrom ::
  ( MonadLogger     m
  , MonadIPFSDaemon m
  , MonadKeyStore   m ExchangeKey
  , MonadIO         m
  , MonadTime       m
  , JWT.Resolver    m
  , MonadBaseControl IO m
  , MonadRescue     m
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
requestFrom targetDID myDID = do
 -- WebSocket class? Linking class?
  control \runInBase ->
    -- Step 1
    WS.runClient "runfission.net" 443 ("/user/link/did:key:z" <> show targetDID) \conn -> do
      runInBase $ reattempt 10 do
        throwawaySK :: RSA.PrivateKey <- KeyStore.generate (Proxy @ExchangeKey)
        throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

        let throwawayDID = DID Key (RSAPublicKey throwawayPK)

        wsSend conn throwawayDID -- STEP 2
        sessionKey <- getAuthenticatedSessionKey conn targetDID throwawaySK -- STEPS 3 & 4
        secureSendPIN conn sessionKey -- STEP 5

        ucan <- listenForFinalUCAN conn sessionKey targetDID myDID -- STEP 6
        storeUCAN ucan

wsSend ::
  ( MonadLogger m
  , MonadIO m
  , ToJSON msg
  , Display msg
  )
  => WS.Connection
  -> msg
  -> m ()
wsSend conn msg = do
  logDebug $ "Pushing over cleartext websocket: " <> textDisplay msg
  liftIO $ WS.sendDataMessage conn (WS.Binary $ encode msg)

storeUCAN :: MonadIO m => JWT -> m ()
storeUCAN = undefined

storeWNFSKeyFor :: Monad m => DID -> FilePath -> Symmetric.Key -> m ()
storeWNFSKeyFor did path aes = undefined

-- STEP 5
secureSendPIN ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` CryptoError
  )
  => WS.Connection
  -> Session.Key
  -> m ()
secureSendPIN conn (Session.Key sessionKey) = do
  pin       <- PIN.create
  securePIN <- Payload.toSecure sessionKey pin
  wsSend conn securePIN

-- FIXME getFinalUCAN or awaitFinalUCAN
listenForFinalUCAN ::
  ( MonadIO      m
  , JWT.Resolver m
  , MonadTime    m
  , MonadLogger  m
  , MonadRaise   m
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` JWT.Error
  , m `Raises` CryptoError
  , m `Raises` String
  )
  => WS.Connection
  -> Session.Key
  -> DID
  -> DID
  -> m UCAN.RawContent
listenForFinalUCAN conn sessionKey targetDID recipientDID =
  reattempt 100 do
    Bearer.Token {..} <- awaitSecureUCAN conn sessionKey
    ensureM $ UCAN.check recipientDID rawContent jwt
    UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot jwt

    if sender == targetDID
      then return rawContent
      else raise "no ucan" -- FIXME

getAuthenticatedSessionKey ::
  ( MonadIO      m
  , MonadLogger  m
  , MonadRandom  m
  , MonadTime    m
  , JWT.Resolver m
  , MonadRescue  m
  , m `Raises` RSA.Error
  , m `Raises` String
  , m `Raises` CryptoError
  , m `Raises` JWT.Error
  )
  => WS.Connection
  -> DID
  -> RSA.PrivateKey
  -> m Session.Key
getAuthenticatedSessionKey conn targetDID sk = do
  logDebug @Text "Listening for authenticated session key"

  attempt go >>= \case
    Left  _   -> go
    Right key -> return key
  where
    go = do
      -- STEP 3
      sessionKey <- listenForSessionKey conn sk

      -- STEP 4
      listenForValidProof conn sessionKey targetDID

      -- Bootstrapped & validated session key
      return sessionKey

wsReceiveJSON :: (MonadIO m, FromJSON a) => WS.Connection -> m (Either String a)
wsReceiveJSON conn = eitherDecode <$> wsReceive conn

wsReceive :: MonadIO m => WS.Connection -> m Lazy.ByteString
wsReceive conn =
  liftIO (WS.receiveDataMessage conn) <&> \case
    WS.Text   bs _ -> bs
    WS.Binary bs   -> bs

-- STEP 3
listenForSessionKey ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadRescue m
  , m `Raises` RSA.Error
  , m `Raises` String -- FIXME better error
  )
  => WS.Connection
  -> RSA.PrivateKey
  -> m Session.Key
listenForSessionKey conn sk =
  reattempt 100 do
    secretMsg <- Lazy.toStrict <$> wsReceive conn

    -- FIXME probably move to own module in core
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

listenForValidProof ::
  ( MonadIO      m
  , MonadLogger  m
  , MonadTime    m
  , JWT.Resolver m
  , MonadRaise   m
  , m `Raises` JWT.Error
  , m `Raises` String -- FIXME better error
  , m `Raises` CryptoError
  )
  => WS.Connection
  -> Session.Key
  -> DID
  -> m UCAN.JWT
listenForValidProof conn sessionKey@(Session.Key (Symmetric.Key rawKey)) targetDID = do
  logDebug @Text "Lisening for valid UCAN proof"

  Bearer.Token {..} <- awaitSecureUCAN conn sessionKey -- FIXME rename to popSecureMsg

  case (jwt |> claims |> potency) == AuthNOnly of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True ->
      case (jwt |> claims |> facts) of
        [] ->
          raise "No facts" -- FIXME

        (Unknown aesFact : _) ->
          case encodeUtf8 aesFact == rawKey of
            False -> raise "Sesison key doesn't match! ABORT!"
            True  -> ensureM $ UCAN.check targetDID rawContent jwt

awaitSecureUCAN ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` String
  , m `Raises` CryptoError
  , FromJSON a
  )
  => WS.Connection
  -> Session.Key
  -> m a
awaitSecureUCAN conn (Session.Key aes256) = do
  -- FIXME maybe just ignore bad messags rather htan blowing up? Or retry?
  -- FIXME or at caller?
  Session.Payload
    { secretMessage = secretMsg@(EncryptedPayload ciphertext)
    , iv
    } <- ensureM $ wsReceiveJSON conn

  case Symmetric.decrypt aes256 iv secretMsg of
    Left err -> do
      -- FIXME MOVE THIS PART TO the decrypt function, even it that means wrapping in m
      logDebug $ "Unable to decrypt message via AES256: " <> decodeUtf8Lenient ciphertext
      raise err

    Right clearBS ->
      case eitherDecodeStrict $ "Bearer " <> clearBS of -- FIXME total hack
        -- FIXME better "can't decode JSON" error
        Left err -> do
          logDebug $ "Unable to decode AES-decrypted message. Raw = " <> decodeUtf8Lenient clearBS
          raise err

        Right bearer ->
          return bearer
