module Fission.CLI.IPFS.Executable
  ( place
  , place'
  , download
  , unpack
  , configure
  ) where

import qualified RIO.ByteString.Lazy            as Lazy

import qualified Codec.Archive.Tar              as Tar
import qualified Codec.Compression.GZip         as GZip

import qualified Turtle

import           Network.IPFS
import qualified Network.IPFS.Process.Error     as IPFS
import           Network.IPFS.Types             as IPFS

import           Servant.Client

import           Fission.Prelude

import           Fission.Error.NotFound.Types

import           Fission.CLI.Environment        as Env
import qualified Fission.CLI.Environment.OS     as OS
import qualified Fission.CLI.Environment.Path   as Path

import qualified Fission.CLI.File               as File

import qualified Fission.CLI.IPFS.Configure     as IPFS.Config
import qualified Fission.CLI.IPFS.Download      as IPFS
import qualified Fission.CLI.IPFS.Release.Types as IPFS
import qualified Fission.CLI.IPFS.Version.Types as IPFS

import           Fission.CLI.GitHub.Class       as GitHub

place ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadGitHub      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadRescue      m
  , m `Raises` OS.Unsupported
  , m `Raises` NotFound FilePath
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
  , MonadGitHub      m
  , MonadLocalIPFS   m
  , MonadEnvironment m
  , MonadRescue      m
  , m `Raises` ClientError
  , m `Raises` NotFound FilePath
  , m `Raises` IPFS.Error
  )
  => OS.Supported
  -> m ()
place' host = do
  logUser $ "🪐 Downloading managed IPFS for " <> textDisplay host
  IPFS.BinPath ipfsPath <- Path.globalIPFSBin

  -- Network
  ipfsBin <- ensureM . unpack =<< download (IPFS.Version 0 12 2) host

  logDebug @Text "🚎 Moving IPFS into place..."
  File.lazyForceWrite ipfsPath ipfsBin
  void $ Turtle.chmod Turtle.executable $ Turtle.decodeString ipfsPath

  configure

download ::
  ( MonadGitHub      m
  , MonadLogger      m
  , MonadRaise       m
  , m `Raises` ClientError
  )
  => IPFS.Version
  -> OS.Supported
  -> m Lazy.ByteString
download version os = do
  logDebug $ "⬇️  Downloading go-ipfs " <> display version <> " for " <> display os
  ensureM . GitHub.sendRequest $ IPFS.getRelease IPFS.Release {..}

unpack :: (MonadIO m, MonadLogger m) => Lazy.ByteString -> m (Either (NotFound FilePath) Lazy.ByteString)
unpack tarGz = do
  logDebug @Text "💗 Unpacking archive..."
  tarGz
    |> GZip.decompress
    |> Tar.read
    |> Tar.foldEntries getIPFS (Left NotFound) (\_ -> Left NotFound)
    |> pure
  where
    getIPFS :: Tar.Entry -> Either (NotFound FilePath) Lazy.ByteString -> Either (NotFound FilePath) Lazy.ByteString
    getIPFS entry acc =
      case (Tar.entryPath entry, Tar.entryContent entry) of
        ("go-ipfs/ipfs", Tar.NormalFile ipfsBin _) -> Right ipfsBin
        _                                          -> acc

configure ::
  ( MonadEnvironment m
  , MonadLocalIPFS   m
  , MonadLogger      m
  , MonadRescue      m
  , m `Raises` IPFS.Error
  )
  => m ()
configure = do
  logUser @Text "🎛️  Configuring managed IPFS"
  void IPFS.Config.init

  void IPFS.Config.enableRelay
  void IPFS.Config.enableHolePunching

  void IPFS.Config.setApiAddress
  void IPFS.Config.setBootstrap
  void IPFS.Config.setGatewayAddress
  void IPFS.Config.setSwarmAddresses
