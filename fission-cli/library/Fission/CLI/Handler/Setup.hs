module Fission.CLI.Handler.Setup (setup) where

import qualified Crypto.PubKey.Ed25519             as Ed25519

import           Network.DNS
import           Network.IPFS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8             as UTF8

import           Fission.Error.NotFound.Types
import           Fission.User.DID.Types
import           Fission.Web.Client.Class
import           Fission.Web.Client.HTTP.Class

import           Fission.CLI.Environment           as Env
import qualified Fission.CLI.Environment.OS        as OS

import qualified Fission.CLI.Display.Success       as Display
import qualified Fission.CLI.IPFS.Executable       as Executable

import           Fission.CLI.Key.Store             as Key

import           Fission.Authorization.ServerDID
import qualified Fission.CLI.Handler.User.Register as User
import           Fission.Error
import           Fission.Web.Auth.Token
import           Fission.Web.Client                as Client

setup ::
  ( MonadIO                  m
  , MonadEnvironment         m
  , MonadWebClient           m
  , MonadManagedHTTP         m
  , MonadLocalIPFS           m
  , MonadTime                m
  , MonadLogger              m
  , MonadKeyStore SigningKey m -- FIXME flip order?
  , MonadWebAuth             m Token
  , MonadWebAuth             m Ed25519.SecretKey
  , ServerDID                m

  , MonadCleanup m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , m `Raises` AlreadyExists Ed25519.SecretKey

  , IsMember ClientError (Errors m)
  , IsMember Key.Error (Errors m)
  , Show (OpenUnion (Errors m))
  )
  => Maybe OS.Supported
  -> BaseUrl
  -> m ()
setup maybeOS fissionURL = do
  Key.create (Proxy @SigningKey)
  username <- User.register

  UTF8.putText "Setting default config..."
  Env.init username fissionURL

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  Display.putOk "Done"
