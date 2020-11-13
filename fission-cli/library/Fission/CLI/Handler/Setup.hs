module Fission.CLI.Handler.Setup (setup) where

import qualified RIO.ByteString                            as BS

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519                     as Ed25519
import qualified Crypto.PubKey.RSA.Types                   as RSA

import           Network.DNS                               as DNS
import           Network.IPFS
import qualified Network.IPFS.Process.Error                as IPFS.Process
import qualified Network.IPFS.Process.Error                as IPFS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8                     as UTF8

import           Fission.Error
import           Fission.Error.NotFound.Types

import           Fission.Security.EncryptedWith.Types

import           Fission.Authorization.ServerDID

import           Fission.User.DID.Types
import           Fission.User.Username.Types

import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import           Fission.Web.Client.Class
import           Fission.Web.Client.HTTP.Class

import           Fission.Web.Auth.Token
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import           Fission.Web.Client                        as Client

import           Fission.CLI.Environment                   as Env
import qualified Fission.CLI.Environment.OS                as OS

import qualified Fission.CLI.Display.Success               as Display
import qualified Fission.CLI.IPFS.Executable               as Executable

import           Fission.CLI.Key.Store                     as Key

import qualified Fission.CLI.Handler.User.Link.Request     as Link
import qualified Fission.CLI.Handler.User.Register         as User

import qualified Fission.IPFS.PubSub.Subscription          as Sub
import qualified Fission.IPFS.PubSub.Subscription          as IPFS.PubSub.Subscription

import qualified Fission.IPFS.PubSub.Session.Key.Types     as Session
import qualified Fission.IPFS.PubSub.Session.Payload       as Session
import           Fission.Web.Auth.Token.JWT                as JWT
import qualified Fission.Web.Auth.Token.JWT                as UCAN
import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Validation     as UCAN
import qualified Fission.Web.Auth.Token.UCAN               as UCAN

import           Fission.CLI.IPFS.Daemon                   as IPFS.Daemon

setup ::
  ( MonadIO           m
  , MonadBaseControl IO m
  , MonadEnvironment  m
  , MonadWebClient    m
  , MonadManagedHTTP  m
  , MonadLocalIPFS    m
  , MonadIPFSDaemon   m
  , MonadTime         m
  , MonadLogger       m
  , MonadKeyStore     m SigningKey
  , MonadWebAuth      m (SecretKey SigningKey)
  , MonadWebAuth      m Token
  , ServerDID         m
  , JWT.Resolver      m

  , MonadCleanup m

  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` IPFS.Error
  , m `Raises` Key.Error
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , m `Raises` AlreadyExists Ed25519.SecretKey
  , m `Raises` NotFound DID
  , m `Raises` DNS.DNSError
  , m `Raises` CryptoError
  , m `Raises` IPFS.Process.Error
  , m `Raises` String
  , m `Raises` RSA.Error
  , m `Raises` JWT.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` Key.Error

  , IsMember ClientError (Errors m)
  , IsMember Key.Error (Errors m)
  , Show (OpenUnion (Errors m))
  )
  => Maybe OS.Supported
  -> BaseUrl
  -> m ()
setup maybeOS fissionURL = do
  Key.create $ Proxy @SigningKey
  Key.create $ Proxy @ExchangeKey

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  UTF8.putText "👤 If you have an existing account, enter the username. [Enter for new account]: "
  username <- BS.getLine >>= \case
                "" -> do
                  logDebug @Text "Setting up new account"
                  User.register

                uNameBS -> do
                  let uName = Username $ decodeUtf8Lenient uNameBS
                  logDebug $ "Atempting link to: " <> textDisplay uName
                  Link.requestRoot uName
                  return uName

  UTF8.putText "Setting default config..."
  Env.init username fissionURL

  Display.putOk "Done"
