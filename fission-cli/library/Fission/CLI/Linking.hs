module Fission.CLI.Linking where

import qualified System.Console.ANSI                       as ANSI

import           Crypto.Cipher.AES                         (AES256)
import           Crypto.Cipher.Types
import           Crypto.Error                              (CryptoError (..),
                                                            CryptoFailable (..))
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                    as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types                       as CRT

import           Data.ByteArray                            as ByteArray

import qualified RIO.ByteString                            as BS
import qualified RIO.ByteString.Lazy                       as Lazy
import qualified RIO.Text                                  as Text

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Fission.Prelude

import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Challenge.Types
import           Fission.Security.EncryptedWith.Types

import           Fission.IPFS.PubSub.Topic
import           Fission.User.DID.Types                    as DID

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                     as Symmetric

import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription
import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Topic                 as IPFS.PubSub

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import qualified Fission.CLI.Display.Text                  as Display
import           Fission.CLI.Environment.Class
import           Fission.CLI.Key.Store                     as KeyStore
import qualified Fission.CLI.Prompt                        as CLI.Prompt

import qualified Fission.CLI.Linking.Status.Types          as Linking

import qualified Fission.IPFS.PubSub.Publish               as Publish
import qualified Fission.IPFS.PubSub.Session.Key.Types     as Session
import qualified Fission.IPFS.PubSub.Session.Payload       as Session

import           Fission.IPFS.PubSub.Subscription.Secure   as Secure


listenToLinkRequests ::
  ( MonadLogger      m
  , MonadKeyStore    m ExchangeKey
  , MonadLocalIPFS   m
  , MonadIO          m
  , MonadEnvironment m
  , MonadCleanup     m
  , m `Sub.SubscribesTo` DID
  , m `Sub.SubscribesTo` Session.Payload Challenge
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` Error
  )
  => DID
  -> m ()
listenToLinkRequests targetDID = do
  -- FIXME If root device, plz check first
  machineSK <- KeyStore.fetch    (Proxy @SigningKey)
  machinePK <- KeyStore.toPublic (Proxy @SigningKey) machineSK

  let
    machineDID =
      DID Key (Ed25519PublicKey machinePK)

  -- case machineDID == targetDID of
    -- True ->

  let
    topic :: IPFS.PubSub.Topic
    topic = IPFS.PubSub.Topic ("deviceLinking#" <> textDisplay targetDID)

  reattempt 100 do
    DID Key pk     <- waitToReceive topic
    reqExchangeKey <- case pk of
                          RSAPublicKey pk' -> return pk'
                          _                -> raise "Not an RSA key" -- FIXME

    sessionKey       <- Session.Key <$> Symmetric.genAES256
    secretSessionKey <- ensureM $ RSA.OAEP.encrypt oaepParams reqExchangeKey (Lazy.toStrict $ encode sessionKey)

    Publish.sendSecure topic sessionKey $ decodeUtf8Lenient secretSessionKey

    requestorDID :: DID <- waitToReceive topic

    Publish.sendSecure topic sessionKey $ (undefined :: UCAN.JWT) -- FIXME UCAN minus potency

    pin <- waitToReceiveSecure sessionKey topic
    confirmChallenge pin >>= \case
      False ->
        Publish.sendSecure topic sessionKey Linking.Denied

      True -> do
        confirmUCANDelegation requestorDID

        delegatedUCAN :: UCAN.JWT <- delegateAllTo requestorDID
        Publish.sendSecure topic sessionKey delegatedUCAN

confirmChallenge ::
  ( MonadCleanup m
  , MonadLogger m
  , MonadIO m
  )
  => Challenge
  -> m Bool
confirmChallenge (Challenge pinTxt) =
  Display.colourized [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow] do
    UTF8.putText "⚠️⚠️ SECURITY CHECK"

    CLI.Prompt.reaskYN $ Text.intercalate " "
      [ "🔢 Confirm that the following Challenge code is from your other device:"
      , pinTxt
      , "[Y/n]"
      ]

-- {linkStatus: DENIED}
confirmUCANDelegation = undefined

delegateAllTo did = do
  undefined

listenForRequestorExchangeDID = do
  undefined

waitToReceive :: (MonadIO m, m `Sub.SubscribesTo` a) => Topic -> m a
waitToReceive topic =
  IPFS.PubSub.Subscription.withQueue topic \tq -> do
    Sub.Message {payload} <- liftIO . atomically $ readTQueue tq
    return payload

waitToReceiveSecure ::
  forall m a .
  ( MonadIO     m
  , MonadLogger m
  , MonadRescue m
  , m `Sub.SubscribesTo` Session.Payload a
  , m `Raises` String
  , m `Raises` CryptoError
  , FromJSON a
  )
  => Session.Key
  -> Topic
  -> m a
waitToReceiveSecure sessionKey topic =
  IPFS.PubSub.Subscription.withQueue topic \tq -> go tq
  where
    go :: TQueue (Sub.Message (Session.Payload a)) -> m a
    go tq = undefined
     --  attempt (Secure.popMessage sessionKey tq) >>= \case
     --    Left  _   -> go tq
     --    Right val -> return val

fetchUCANForDID = undefined
