module Fission.CLI.Linking.Request (requestFrom) where

import qualified Fission.Key.Symmetric.AES256.Payload.Types as AES256
import           Fission.PubSub                             as PubSub
import qualified Fission.PubSub.DM.Channel.Types            as DM
import           Fission.PubSub.Secure                      as PubSub.Secure
import           Servant.Client.Core

import           Crypto.Cipher.AES                          (AES256)
import qualified Fission.Key.Symmetric                      as Symmetric

import           Data.ByteArray                             as ByteArray

import           Crypto.Error
import           Crypto.Hash.Algorithms
import qualified Crypto.PubKey.RSA.OAEP                     as RSA.OAEP
import qualified Crypto.PubKey.RSA.Types                    as RSA
import           Crypto.Random.Types

import qualified RIO.ByteString.Lazy                        as Lazy
import qualified RIO.Text                                   as Text

import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                    as RSA
import           Crypto.Random.Types

import           Network.IPFS.Local.Class                   as IPFS
import qualified Network.IPFS.Process.Error                 as IPFS.Process

import qualified Network.WebSockets                         as WS

import           Fission.Prelude

import           Fission.Key.Asymmetric.Public.Types
import qualified Fission.Key.Symmetric                      as Symmetric

import           Fission.User.DID.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.Potency.Types
import           Fission.Web.Auth.Token.JWT                 as JWT
import qualified Fission.Web.Auth.Token.JWT                 as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error           as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class  as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error  as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation      as UCAN
import qualified Fission.Web.Auth.Token.UCAN                as UCAN

import qualified Fission.IPFS.PubSub.Session.Payload        as Session

import qualified Fission.IPFS.PubSub.Subscription           as Sub
import qualified Fission.IPFS.PubSub.Subscription           as IPFS.PubSub.Subscription
import           Fission.IPFS.PubSub.Topic

import           Fission.CLI.Key.Store                      as KeyStore
import qualified Fission.CLI.Linking.PIN                    as PIN

import           Fission.CLI.IPFS.Daemon                    as IPFS.Daemon

import qualified Fission.IPFS.PubSub.Publish                as Publish
import qualified Fission.IPFS.PubSub.Subscription.Secure    as Secure

import           Fission.IPFS.PubSub.Session.Payload        as Payload
import           Fission.Security.EncryptedWith.Types

import           Fission.Web.Auth.Token.Bearer.Types        as Bearer

requestFrom ::
  ( MonadLogger       m
  , MonadIPFSDaemon   m
  , MonadKeyStore     m ExchangeKey
  , MonadIO           m
  , MonadTime         m
  , JWT.Resolver      m
  , MonadRescue       m
  , MonadPubSub       m
  , MonadPubSubSecure m (Symmetric.Key AES256)
  , MonadPubSubSecure m RSA.PrivateKey
  , MonadPubSubSecure m DM.Channel
  , ToJSON (SecurePayload m (Symmetric.Key AES256) PIN.PIN)
  , ToJSON (SecurePayload m (Symmetric.Key AES256) Token)
  , FromJSON (SecurePayload m (Symmetric.Key AES256) Token)
  , FromJSON (SecurePayload m RSA.PrivateKey (Symmetric.Key AES256))
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
  PubSub.connect baseURL topic \conn -> reattempt 10 do
    aesKey <- secureConnection conn () \rsaConn@SecureConnection {key} ->
      reattempt 10 do
        broadcast conn $ DID Key (RSAPublicKey $ RSA.private_pub key) -- STEP 2

        reattempt 10 do
          -- STEP 3
          aesKey      <- secureListen rsaConn
          bearerToken <- secureListen SecureConnection {conn, key = aesKey}-- sessionKey

          -- STEP 4
          validateProof bearerToken targetDID myDID aesKey
          return aesKey

    let aesConn = SecureConnection {conn, key = aesKey}

    -- Step 5
    pin <- PIN.create
    secureBroadcast aesConn pin

    -- STEP 6
    reattempt 100 do
      Bearer.Token {..} <- secureListen aesConn
      ensureM $ UCAN.check myDID rawContent jwt
      UCAN.JWT {claims = UCAN.Claims {sender}} <- ensureM $ UCAN.getRoot jwt

      if sender == targetDID
        then storeUCAN rawContent
        else raise "no ucan" -- FIXME

  where
    topic   = PubSub.Topic $ textDisplay targetDID
    baseURL = BaseUrl Https "runfission.net" 443 "/user/link"

validateProof ::
  ( MonadIO      m
  , MonadLogger  m
  , MonadTime    m
  , JWT.Resolver m
  , MonadRaise   m
  , m `Raises` JWT.Error
  , m `Raises` String -- FIXME better error
  , m `Raises` CryptoError
  )
  => Bearer.Token
  -> DID
  -> DID
  -> Symmetric.Key AES256
  -> m UCAN.JWT
validateProof Bearer.Token {..} myDID targetDID sessionAES = do
  ensureM $ UCAN.check myDID rawContent jwt

  case (jwt |> claims |> potency) == AuthNOnly of
    False ->
      raise "Not a closed UCAN" -- FIXME

    True ->
      case (jwt |> claims |> facts) of
        [] ->
          raise "No facts" -- FIXME

        (SessionKey aesFact : _) ->
          case aesFact == sessionAES of
            False -> raise "Sesison key doesn't match! ABORT!"
            True  -> ensureM $ UCAN.check targetDID rawContent jwt

storeUCAN :: MonadIO m => UCAN.RawContent -> m ()
storeUCAN = undefined

storeWNFSKeyFor :: Monad m => DID -> FilePath -> Symmetric.Key AES256 -> m ()
storeWNFSKeyFor did path aes = undefined
