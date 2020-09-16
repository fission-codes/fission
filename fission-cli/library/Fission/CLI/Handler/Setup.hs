module Fission.CLI.Handler.Setup (setup) where

import           Crypto.Random
import           Network.DNS
import           Network.IPFS.Remote.Class
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8        as UTF8

import           Fission.Error.NotFound.Types
import           Fission.User.DID.Types
import           Fission.Web.Client.Class

import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Environment.OS   as OS

import qualified Fission.CLI.Display.Success  as Display
import qualified Fission.CLI.IPFS.Executable  as Executable

import qualified Fission.CLI.Key.Store        as Key

setup ::
  ( MonadIO          m
  , MonadRemoteIPFS  m
  , MonadEnvironment m
  , MonadWebClient   m
  , MonadLogger      m
  , MonadRandom      m
  , MonadCleanup     m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , m `Raises` DNSError
  , m `Raises` NotFound DID
  , Show (OpenUnion (Errors m))
  )
  => Maybe OS.Supported
  -> BaseUrl
  -> m ()
setup maybeOS fissionURL = do
  UTF8.putText "Setting default config..."
  Env.init fissionURL

  UTF8.putText "Creating key..."
  Key.create

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  Display.putOk "Done"
