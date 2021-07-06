module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy                    as Lazy
-- import qualified RIO.Text                       as Text
import           RIO.FilePath                           ((</>))

import qualified Network.HTTP.Client                    as HTTP

import qualified Codec.Archive.Tar                      as Tar
import qualified Codec.Compression.GZip                 as GZip

import qualified Turtle

import           Network.IPFS
-- import qualified Network.IPFS.File.Types        as File
import qualified Network.IPFS.Process.Error             as IPFS
import           Network.IPFS.Types                     as IPFS

import           Servant.API
import           Servant.Client

import           Fission.Prelude

import           Fission.Web.Client.HTTP.Class

-- import qualified Fission.CLI.Environment.IPFS   as IPFS

-- import           Fission.CLI.Bootstrap
import           Fission.CLI.Environment                as Env
import qualified Fission.CLI.Environment.OS             as OS
import qualified Fission.CLI.Environment.Path           as Path

-- import           Fission.CLI.File
import qualified Fission.CLI.IPFS.Configure             as IPFS.Config
import qualified Fission.CLI.IPFS.Version.Types         as IPFS

import           Fission.CLI.IPFS.Download.GitHub.Types

import           Fission.CLI.GitHub.Class               as GitHub

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

  -- FIXME File.Serialized lazyFile <- ensureM . download $ IPFS.binCidFor host

  -- ipfsPath `forceWrite` Lazy.toStrict lazyFile


  logUser @Text "🎛️  Configuring managed IPFS"

  void IPFS.Config.init
  void IPFS.Config.enableRelay

  void IPFS.Config.setApiAddress
  void IPFS.Config.setBootstrap
  void IPFS.Config.setGatewayAddress
  void IPFS.Config.setSwarmAddresses

download ::
  ( MonadEnvironment m
  , MonadGitHub      m
  , MonadIO          m
  , MonadRaise       m
  , m `Raises` ClientError
  )
  => IPFS.Version
  -> FilePath
  -> m ()
download ipfsVersion ipfsPath = do
  -- Environment
  IPFS.BinPath ipfsPath <- Path.globalIPFSBin
  tmp                   <- Path.globalTmpDir

  -- Network
  tarGz <- ensureM $ GitHub.sendRequest foo

  -- FileSystem
  Lazy.writeFile (tmp </> tmpTar) (GZip.decompress tarGz)
  liftIO $ Tar.extract (tmp </> tmpTar) (tmp </> tmpBinDir)
  Turtle.mv (Turtle.decodeString (tmp </> tmpBinDir </> "ipfs")) (Turtle.decodeString ipfsPath)
  void . Turtle.chmod Turtle.executable $ Turtle.decodeString ipfsPath

  where
    baseUrl :: BaseUrl
    baseUrl = BaseUrl Https "github.com" 443 ""

    foo :: ClientM Lazy.ByteString
    foo = client (Proxy @GetRelease) ipfsVersion "dhsaj" -- FIXME

    tmpTar :: FilePath
    tmpTar = "go-ipfs-release.tar.gz"

    tmpBinDir :: FilePath
    tmpBinDir = "go-ipfs-release"
