module Fission.CLI.Handler.Setup (setup) where

import qualified RIO.ByteString                    as BS

-- import           Crypto.Error
import qualified Crypto.PubKey.Ed25519             as Ed25519
-- import qualified Crypto.PubKey.RSA.Types                   as RSA
import           Crypto.Random

import           Network.IPFS
-- import qualified Network.IPFS.Process.Error        as IPFS
import qualified Network.IPFS.Process.Error        as IPFS.Process

import           Network.DNS                       as DNS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8             as UTF8

import           Fission.Error

import           Fission.User.DID.Types
import qualified Fission.User.Username.Error       as Username

import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Authorization.ServerDID

import           Fission.Web.Client                as Client
import           Fission.Web.Client.HTTP.Class

-- import qualified Fission.Web.Auth.Token.JWT.Error          as JWT
-- import qualified Fission.Web.Auth.Token.JWT.Resolver.Class as JWT
-- import qualified Fission.Web.Auth.Token.JWT.Resolver.Error as UCAN.Resolver
import           Fission.Web.Auth.Token.Types

import           Fission.CLI.Environment           as Env
import qualified Fission.CLI.Environment.OS        as OS

import qualified Fission.CLI.Display.Success       as Display
import qualified Fission.CLI.IPFS.Executable       as Executable
import           Fission.CLI.Key.Store             as Key

import qualified Fission.CLI.Handler.User.Register as User

setup ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadManagedHTTP m
  , MonadLocalIPFS   m
  , MonadTime        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadWebAuth     m Token
  , MonadWebAuth     m (SecretKey SigningKey)
  , ServerDID        m

  , MonadCleanup m
  , m `Raises` AlreadyExists Ed25519.SecretKey
  , m `Raises` ClientError
  , m `Raises` DNSError
  , m `Raises` IPFS.Process.Error
  , m `Raises` Key.Error
  , m `Raises` NotFound DID
  , m `Raises` OS.Unsupported
  , m `Raises` Username.Invalid

  , IsMember ClientError (Errors m)
  , Show (OpenUnion (Errors m))
  , Errors m `Contains` Errors m
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
  Env.init username fissionURL

  Display.putOk "Done"

  where
    getUsername "" = do
      logDebug @Text "Setting up new account"
      User.register maybeUsername maybeEmail

    getUsername usernameBS = do
      uName <- ensure . mkUsername $ decodeUtf8Lenient usernameBS
      logDebug $ "Atempting link to: " <> textDisplay uName
      -- FIXME add back in when link handler exists
      -- Link.requestRoot uName
      return uName
