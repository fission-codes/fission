module Fission.CLI.Handler.Setup (setup) where

import qualified Crypto.PubKey.Ed25519             as Ed25519
import           Crypto.Random
import           Network.DNS
import           Network.IPFS.Remote.Class
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8             as UTF8

import           Fission.Error.NotFound.Types
import           Fission.User.DID.Types
import           Fission.Web.Client.Class

import           Fission.CLI.Environment           as Env
import qualified Fission.CLI.Environment.OS        as OS

import qualified Fission.CLI.Display.Success       as Display
import qualified Fission.CLI.IPFS.Executable       as Executable

import qualified Fission.CLI.Key.Store             as Key


import           Network.HTTP.Client               as HTTP
import           Network.IPFS
import qualified Network.IPFS.BinPath.Types        as IPFS
import qualified Network.IPFS.File.Types           as File
import           Network.IPFS.Types                as IPFS
import           Servant.Client

import           Fission.Authorization.ServerDID
import qualified Fission.CLI.Handler.User.Register as User
import           Fission.Error
import           Fission.Web.Auth.Token
import           Fission.Web.Client                as Client

setup ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadTime        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadWebAuth     m Token
  , MonadWebAuth     m Ed25519.SecretKey
  , ServerDID        m
  -- FIXME deep annoyance
  , MonadReader cfg m
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "ipfsURL"     cfg IPFS.URL
  , Generic cfg
  --
  , MonadCleanup m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , m `Raises` AlreadyExists Ed25519.SecretKey

  , IsMember OS.Unsupported (Errors m)
  , IsMember ClientError (Errors m)
  , IsMember Key.Error (Errors m)
  , IsMember DNSError (Errors m)
  , NotFound DID `IsMember` (Errors m)
  , AlreadyExists Ed25519.SecretKey `IsMember` Errors m
  , Show (OpenUnion (Errors m))
  )
  => Maybe OS.Supported
  -> BaseUrl
  -> m ()
setup maybeOS fissionURL = do
  username <- User.register

  UTF8.putText "Setting default config..."
  Env.init username fissionURL

  UTF8.putText "Creating key..."
  Key.create

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  Display.putOk "Done"
