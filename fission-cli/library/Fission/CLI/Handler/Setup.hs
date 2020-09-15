module Fission.CLI.Handler.Setup (setup) where

import           Crypto.Random
import           Network.IPFS.Remote.Class
import           Servant.Client

import           Fission.Prelude

import qualified Fission.Internal.UTF8           as UTF8

import           Fission.Authorization.ServerDID
import           Fission.Web.Client.Class

import           Fission.CLI.Environment         as Env
import qualified Fission.CLI.Environment.OS      as OS

import qualified Fission.CLI.Display.Success     as Display
import qualified Fission.CLI.IPFS.Executable     as Executable

import qualified Fission.CLI.Key.Store           as Key

setup ::
  ( MonadIO          m
  , MonadRemoteIPFS  m
  , MonadEnvironment m
  , MonadWebClient   m
  , ServerDID        m
  , MonadLogger      m
  , MonadRandom      m
  , MonadCleanup     m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` Key.Error
  , Show (OpenUnion (Errors m))
  )
  => Maybe OS.Supported
  -> m ()
setup maybeOS = do
  UTF8.putText "Setting default config..."
  Env.init

  UTF8.putText "Creating key..."
  Key.create

  UTF8.putText "Installing dependencies..."
  Executable.place maybeOS

  Display.putOk "Done"
