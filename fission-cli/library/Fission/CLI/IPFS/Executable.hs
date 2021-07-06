module Fission.CLI.IPFS.Executable
  ( place
  , place'
  , download
  , unpack
  , configure
  ) where

import qualified RIO.ByteString.Lazy            as Lazy
import           RIO.Directory
import           RIO.FilePath                   (dropFileName, (</>))

import qualified Codec.Archive.Tar              as Tar
import qualified Codec.Compression.GZip         as GZip

import qualified Turtle

import           Network.IPFS
import qualified Network.IPFS.Process.Error     as IPFS
import           Network.IPFS.Types             as IPFS

import           Servant.Client

import           Fission.Prelude

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
  , m `Raises` IPFS.Error
  )
  => OS.Supported
  -> m ()
place' host = do
  logUser $ "🪐 Downloading managed IPFS for " <> textDisplay host
  unpack =<< download (IPFS.Version 0 9 0) host
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

unpack :: (MonadIO m, MonadLogger m, MonadEnvironment m) => Lazy.ByteString -> m ()
unpack tarGz = do
  IPFS.BinPath ipfsPath <- Path.globalIPFSBin
  tmp                   <- Path.globalTmpDir

  let
    tmpTar      = tmp </> "go-ipfs-release.tar.gz"
    source      = Turtle.decodeString (tmp </> "go-ipfs" </> "ipfs")
    destination = Turtle.decodeString ipfsPath

  -- FileSystem
  logDebug @Text "💗 Decompressing archive..."
  File.forceWrite tmpTar (Lazy.toStrict $ GZip.decompress tarGz)

  logDebug @Text "🦖 Untarring..."
  liftIO $ Tar.extract tmp tmpTar
  void $ Turtle.chmod Turtle.executable source

  logDebug @Text "🚎 Moving IPFS into place..."
  createDirectoryIfMissing True $ dropFileName ipfsPath
  Turtle.mv source destination

  logDebug @Text "🧹 Cleaning up..."
  Turtle.rmtree $ Turtle.decodeString tmp

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

  void IPFS.Config.setApiAddress
  void IPFS.Config.setBootstrap
  void IPFS.Config.setGatewayAddress
  void IPFS.Config.setSwarmAddresses
