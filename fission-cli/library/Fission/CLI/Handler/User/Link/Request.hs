-- | Linking requests (asking for a UCAN)
module Fission.CLI.Handler.User.Link.Request (requestRoot) where

import qualified Data.ByteString.Char8                     as BS8

import qualified Network.DNS                               as DNS
import qualified Network.IPFS.Process.Error                as IPFS.Process

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.Key.Asymmetric.Public.Types
import           Fission.Key.Error                         as Key

import           Fission.User.DID.Types
import           Fission.User.Username.Types

import qualified Fission.CLI.Display.Error                 as CLI.Error

import qualified Fission.CLI.Linking.Request               as Linking


import           Crypto.Error
import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random.Types
import qualified Fission.CLI.Key.Store                     as Key.Store
import           Fission.CLI.Key.Store.Types

import           Network.IPFS.Local.Class                  as IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process

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

import qualified Fission.IPFS.PubSub.Publish               as Publish
import qualified Fission.IPFS.PubSub.Subscription.Secure   as Secure


import           Fission.CLI.Environment.Class

requestRoot ::
  ( MonadIO        m
  , MonadLogger    m
  , MonadKeyStore  m ExchangeKey
  , MonadLocalIPFS m
  , MonadIO        m
  , MonadTime      m
  , JWT.Resolver   m
  , MonadEnvironment m
  , MonadRescue    m
  , m `Sub.SubscribesTo` EncryptedWith RSA.PrivateKey
  , m `Sub.SubscribesTo` Session.Payload JWT.RawContent
  , m `Raises` NotFound DID
  , m `Raises` DNS.DNSError
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` Key.Error
  )
  => Username
  -> m ()
requestRoot username = do
  targetDID <- getDIDByUsername username

  sk        <- Key.Store.fetch    (Proxy @SigningKey)
  pk        <- Key.Store.toPublic (Proxy @SigningKey) sk

  Linking.requestFrom targetDID (DID Key $ Ed25519PublicKey pk)

getDIDByUsername ::
  ( MonadIO     m
  , MonadLogger m
  , MonadRaise  m
  , m `Raises` NotFound DID
  , m `Raises` DNS.DNSError
  )
  => Username
  -> m DID
getDIDByUsername (Username usernameTxt) = do
  rs <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf

  let url = "_did." <> encodeUtf8 usernameTxt <> ".fission.name"

  logDebug $ "No cached server DID. Fetching from " <> decodeUtf8Lenient url

  liftIO (DNS.withResolver rs \resolver -> DNS.lookupTXT resolver url) >>= \case
    Left errs -> do
      CLI.Error.put errs ("Unable to find DID for: " <> usernameTxt)
      raise errs

    Right [] -> do
      CLI.Error.put (NotFound @DID) $ "No TXT record at " <> decodeUtf8Lenient url
      raise $ NotFound @DID

    Right (didTxt : _) ->
      case eitherDecodeStrict ("\"" <> didTxt <> "\"") of
        Left errs -> do
          CLI.Error.put errs "Unable to find Fission's ID online"
          raise $ NotFound @DID

        Right userDID -> do
          logDebug $ "DID retrieved " <> textDisplay userDID
          return userDID
