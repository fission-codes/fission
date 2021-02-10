module Fission.CLI.Handler.Setup (setup) where

import qualified RIO.ByteString                              as BS

import           Crypto.Cipher.AES                           (AES256)
import qualified Crypto.PubKey.Ed25519                       as Ed25519
import qualified Crypto.PubKey.RSA.Types                     as RSA
import           Crypto.Random

import           Network.IPFS
-- import qualified Network.IPFS.Process.Error        as IPFS
import qualified Network.IPFS.Process.Error                  as IPFS.Process

import           Network.DNS                                 as DNS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8                       as UTF8

import           Fission.Error
import qualified Fission.JSON                                as JSON
import qualified Fission.Key.Symmetric                       as Symmetric

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error                 as Username

import           Fission.User.DID.NameService.Class
import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Authorization.ServerDID

import           Fission.Web.Client                          as Client
import           Fission.Web.Client.HTTP.Class

-- import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
-- import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
-- import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import qualified Fission.Web.Auth.Token.JWT.Error            as JWT
import qualified Fission.Web.Auth.Token.JWT.Proof.Error      as JWT.Proof
import qualified Fission.Web.Auth.Token.JWT.Resolver.Class   as JWT
import qualified Fission.Web.Auth.Token.JWT.Resolver.Error   as UCAN.Resolver
import           Fission.Web.Auth.Token.Types

import           Fission.CLI.Environment                     as Env
import qualified Fission.CLI.Environment.OS                  as OS

import qualified Fission.CLI.Display.Success                 as Display
import qualified Fission.CLI.IPFS.Executable                 as Executable
import           Fission.CLI.Key.Store                       as Key
import qualified Fission.CLI.PIN.Payload.Types               as PIN
import           Fission.CLI.Remote
import qualified Fission.CLI.User.Link.Payload.Types         as User.Link

import           Fission.CLI.PubSub.Secure.Class
import           Fission.CLI.PubSub.Secure.Payload           as SecurePayload
import qualified Fission.CLI.PubSub.Secure.Session.Types     as PubSub

import qualified Fission.CLI.WebNative.FileSystem.Auth.Store as WebNative.FileSystem.Auth.Store
import qualified Fission.CLI.WebNative.Mutation.Auth.Store   as WebNative.Mutation.Store

import qualified Fission.CLI.Handler.User.Login              as User
import qualified Fission.CLI.Handler.User.Register           as User

setup :: forall m .
  ( MonadIO          m
  , MonadTime        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadRemote      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadManagedHTTP m
  , MonadWebClient   m
  , MonadWebAuth     m Token
  , MonadWebAuth     m (SecretKey SigningKey)
  , MonadNameService m
  , ServerDID        m
  , JWT.Resolver     m

  , WebNative.FileSystem.Auth.Store.MonadStore m
  , WebNative.Mutation.Store.MonadStore        m

  , MonadSecured m (Symmetric.Key AES256) PIN.Payload
  , MonadSecured m (Symmetric.Key AES256) User.Link.Payload
  , MonadSecured m (RSA.PublicKey, RSA.PrivateKey) PubSub.Session

  , MonadPubSubSecure m (RSA.PublicKey, RSA.PrivateKey)

  , MonadCleanup m
  , m `Raises` AlreadyExists Ed25519.SecretKey
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` IPFS.Process.Error
  , m `Raises` JSON.Error
  , m `Raises` JWT.Error
  , m `Raises` JWT.Proof.Error
  , m `Raises` Key.Error
  , m `Raises` NotFound DID
  , m `Raises` OS.Unsupported
  , m `Raises` SecurePayload.Error
  , m `Raises` UCAN.Resolver.Error
  , m `Raises` Username.Invalid

  , Errors m `Contains` Errors m
  , IsMember ClientError (Errors m)

  , Show (OpenUnion (Errors m))
  , Display (SecurePayload (Symmetric.Key AES256) User.Link.Payload)
  )
  => Maybe OS.Supported
  -> BaseUrl
  -> Maybe Username
  -> Maybe Email
  -> m ()
setup maybeOS fissionURL maybeUsername maybeEmail = do
  Key.create $ Proxy @SigningKey
  Key.create $ Proxy @ExchangeKey

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  UTF8.putText "👤 If you have an existing account, enter the username. [Enter for new account]: "
  username <- getUsername =<< BS.getLine

  UTF8.putText "Setting default config..."
  Env.init username fissionURL Nothing

  Display.putOk "Done"

  where
    getUsername :: ByteString -> m Username
    getUsername "" = do
      logDebug @Text "Setting up new account"
      User.register maybeUsername maybeEmail

    getUsername usernameBS = do
      uName <- ensure . mkUsername $ decodeUtf8Lenient usernameBS
      User.login uName -- FIXME only inbound
      return uName
