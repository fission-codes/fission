module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy           as Lazy
import qualified RIO.Text                      as Text

import qualified Turtle

import           Network.IPFS
import qualified Network.IPFS.File.Types       as File
import qualified Network.IPFS.Process.Error    as IPFS
import           Network.IPFS.Types            as IPFS

import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client.HTTP.Class

import qualified Fission.CLI.Environment.IPFS  as IPFS

import           Fission.CLI.Bootstrap
import           Fission.CLI.Environment       as Env
import qualified Fission.CLI.Environment.OS    as OS
import qualified Fission.CLI.Environment.Path  as Path

import           Fission.CLI.File
import qualified Fission.CLI.IPFS.Configure    as IPFS.Config

place ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadManagedHTTP m
  , MonadRescue      m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  , m `Raises` IPFS.Error
  )
  => Maybe OS.Supported
  -> m ()
place (Just os) = place' os
place Nothing   = place' =<< ensure OS.get

place' ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadLocalIPFS   m
  , MonadManagedHTTP m
  , MonadEnvironment m
  , MonadRescue      m
  , m `Raises` ClientError
  , m `Raises` IPFS.Error
  )
  => OS.Supported
  -> m ()
place' host = do
  logUser $ "🪐 Downloading managed IPFS for " <> textDisplay host

  IPFS.BinPath    ipfsPath <- Path.globalIPFSBin
  File.Serialized lazyFile <- ensureM . runBootstrapT . ipfsCat $ IPFS.binCidFor host

  logDebug $ "Writing IPFS binary to " <> Text.pack ipfsPath
  ipfsPath `forceWrite` Lazy.toStrict lazyFile

  void . Turtle.chmod Turtle.executable $ Turtle.decodeString ipfsPath

  logUser @Text "🎛️  Configuring managed IPFS"

  void IPFS.Config.init
  void IPFS.Config.enableRelay

  void IPFS.Config.setApiAddress
  void IPFS.Config.setBootstrap
  void IPFS.Config.setGatewayAddress
  void IPFS.Config.setSwarmAddresses
