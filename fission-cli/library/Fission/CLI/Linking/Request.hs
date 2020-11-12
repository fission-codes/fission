module Fission.CLI.Linking.Request
  ( requestFrom
  , broadcastDID
  , getAuthenticatedSessionKey
  , secureSendPIN
  , listenForFinalUCAN
  , listenForSessionKey
  , listenForValidProof
  ) where

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

import           Fission.IPFS.PubSub.Session.Payload.Types

requestFrom ::
  ( MonadLogger     m
  , MonadIPFSDaemon m
  , MonadKeyStore   m ExchangeKey
  , MonadLocalIPFS  m
  , MonadIO         m
  , MonadTime       m
  , JWT.Resolver    m
  , MonadRescue     m
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Sub.SubscribesTo` Session.Payload JWT.RawContent
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
  tqIn  <- atomicallyM newTQueue
  tqOut <- atomicallyM newTQueue

  socket targetDID tqIn tqOut `withAsync` \_ -> do
    reattempt 10 do
      throwawaySK <- KeyStore.generate (Proxy @ExchangeKey)
      throwawayPK <- KeyStore.toPublic (Proxy @ExchangeKey) throwawaySK

      let throwawayDID = DID Key (RSAPublicKey throwawayPK)

      wsSendClear targetDID throwawayDID tqOut -- STEP 2, yes out of order is actually correct
      sessionKey <- getAuthenticatedSessionKey targetDID topic throwawaySK -- STEP 1-4
      secureSendPIN topic sessionKey -- STEP 5

      ucan <- listenForFinalUCAN targetDID myDID topic sessionKey -- STEP 6
      storeUCAN ucan

wsSendClear ::
  ( MonadLogger m
  , ToJSON  msg
  , Display msg
  )
  => DID
  -> msg
  -> TQueue msg
  -> m ()
wsSendClear did msg tqOut = do
  logDebug $ "Pushing over cleartext websocket: " <> textDisplay msg
  atomicallyM $ writeTQueue tqOut msg

wsSendSecure ::
  ( MonadLogger    m
  , MonadRandom    m
  , MonadRaise     m
  , m `Raises` CryptoError
  , ToJSON msg
  )
  => DID
  -> Session.Key
  -> msg
  -> TQueue msg
  -> m ()
wsSendSecure did (Session.Key aesKey) msg tq = do
  encrypted <- Payload.toSecure aesKey msg
  wsSendClear did encrypted tqOut

storeUCAN = undefined

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
  -> Session.Key
  -> m ()
secureSendPIN topic sessionKey =
  Publish.sendSecure topic sessionKey =<< PIN.create

listenForFinalUCAN ::
  ( MonadIO      m
  , JWT.Resolver m
  , MonadTime    m
  , MonadLogger  m
  , MonadRaise   m
  , m `Sub.SubscribesTo` Session.Payload JWT.RawContent
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` JWT.Error
  , m `Raises` CryptoError
  , m `Raises` String
  )
  => DID
  -> DID
  -> Topic
  -> Session.Key
  -> m UCAN.JWT -- FIXME Or the raw bytestirng version? At minimum want to validate internally
listenForFinalUCAN targetDID recipientDID topic sessionKey =
  IPFS.PubSub.Subscription.withQueue topic \tq -> go tq
  where
    go tq = do
      candidateRaw@(UCAN.RawContent raw) <- Secure.popMessage sessionKey tq -- FIXME rename to popSecureMsg

      candidateUCAN <- ensure . eitherDecodeStrict $ encodeUtf8 raw
      ensureM $ UCAN.check recipientDID candidateRaw candidateUCAN

      UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot candidateUCAN
      if sender == targetDID
        then return candidateUCAN
        else go tq

broadcastDID ::
  ( MonadLocalIPFS m
  , MonadLogger    m
  , MonadRaise     m
  , m `Raises` IPFS.Process.Error
  )
  => Topic
  -> DID
  -> m ()
broadcastDID topic did = Publish.sendClear topic did

getAuthenticatedSessionKey ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRandom m
  , MonadTime   m
  , JWT.Resolver m
  , MonadRescue  m
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey -- NOTE SubscribesToChannel & SubscribesToSecure
  , m `Sub.SubscribesTo` Session.Payload JWT.RawContent
  , m `Raises` RSA.Error
  , m `Raises` String
  , m `Raises` CryptoError
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  )
  => DID
  -> Topic
  -> RSA.PrivateKey
  -> m Session.Key
getAuthenticatedSessionKey targetDID topic sk = do
  logDebug @Text "Listening for authenticated session key"

  attempt go >>= \case
    Left  _   -> go
    Right key -> return key
  where
    go = do
      -- STEP 3
      sessionKey <- IPFS.PubSub.Subscription.withQueue topic $ listenForSessionKey sk

      -- STEP 4
      IPFS.PubSub.Subscription.withQueue topic $ listenForValidProof targetDID sessionKey

      -- Bootstrapped & validated session key
      return sessionKey

-- FIXME SUBSCRIBE WSS
socket ::
  ( ToJSON   a
  , FromJSON a
  , MonadIO m
  )
  => DID
  -> TQueue a -- FIXME newtype
  -> TQueue a
  -> m ()
socket did tqIn tqOut =
  -- FIXME get from config
  liftIO $ WS.runClient "runfission.net" 443 ("/user/link/did:key:z" <> show did) \conn -> do
    concurrently_
      (incoming tqOut conn)
      (outgoing tqIn  conn)

outgoing :: ToJSON a => TQueue a -> WS.Connection -> IO ()
outgoing tqIn conn =
  forever do
    msg <- atomically $ readTQueue tqIn
    WS.sendDataMessage conn (WS.Binary $ encode msg)

incoming :: FromJSON a => TQueue a -> WS.Connection -> IO ()
incoming tqOut conn =
  forever do
    rawMsg <- WS.receiveDataMessage conn
    -- FIXME log that you got a message

    let
      bsMsg = case rawMsg of
                WS.Text   bs _ -> bs
                WS.Binary bs   -> bs

    case eitherDecode bsMsg of
      Left  _   -> noop -- Silently fail FIXME?
      Right msg -> atomically $ writeTQueue tq msg

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
  -> m Session.Key
listenForSessionKey throwawaySK tq = Secure.popRSAMessage throwawaySK tq

listenForValidProof ::
  ( MonadIO      m
  , MonadLogger  m
  , MonadTime    m
  , JWT.Resolver m
  , MonadRaise   m
  , m `Raises` JWT.Error
  , m `Raises` String -- FIXME better error
  , m `Raises` CryptoError
  , m `Raises` UCAN.Resolver.Error
  )
  => DID
  -> Session.Key
  -> TQueue (Sub.Message (Session.Payload JWT.RawContent))
  -> m UCAN.JWT
listenForValidProof targetDID sessionKey@(Session.Key (Symmetric.Key rawKey)) tq = do
  logDebug @Text "Lisening for valid UCAN proof"

  candidateRaw@(UCAN.RawContent txt) <- Secure.popMessage sessionKey tq -- FIXME rename to popSecureMsg
  candidateUCAN <- ensure . eitherDecodeStrict $ encodeUtf8 txt

  case (candidateUCAN |> claims |> potency) == AuthNOnly of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True -> do
      case (candidateUCAN |> claims |> facts) of
        [] ->
          raise "No facts" -- FIXME

        (Unknown aesFact : _) -> do
          case encodeUtf8 aesFact == rawKey of
            False ->
              raise "Sesison key doesn't match! ABORT!"

            True -> do
              ensureM $ UCAN.check targetDID candidateRaw candidateUCAN
