module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy           as Lazy
import qualified RIO.Text                      as Text

import qualified Turtle                        as Turtle

import           Network.IPFS
import qualified Network.IPFS.File.Types       as File
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

place ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadManagedHTTP m
  , MonadRescue      m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  )
  => Maybe OS.Supported
  -> m ()
place (Just os) = place' os
place Nothing   = place' =<< ensure OS.get

place' ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadManagedHTTP m
  , MonadEnvironment m
  , MonadRescue      m
  , m `Raises` ClientError
  )
  => OS.Supported
  -> m ()
place' host = do
  logDebug $ "Setting up IPFS binary for " <> textDisplay host

  IPFS.BinPath ipfsPath <- Path.globalIPFSBin
  ipfsRepo              <- Path.globalIPFSRepo

  Turtle.export "IPFS_PATH" $ Text.pack ipfsRepo

  File.Serialized lazyFile <- ensureM $ runBootstrapT do
    ipfsCat $ IPFS.binCidFor host

  logDebug $ "Writing IPFS binary to " <> Text.pack ipfsPath
  ipfsPath `forceWrite` Lazy.toStrict lazyFile

  void . Turtle.chmod Turtle.executable $ Turtle.decodeString ipfsPath

  _exitCode <- runProcess . fromString $ ipfsPath <> " init &> /dev/null"
  _exitCode <- runProcess . fromString $ ipfsPath <> " config Addresses.API     /ip4/0.0.0.0/tcp/10235"
  _exitCode <- runProcess . fromString $ ipfsPath <> " config Addresses.Gateway /ip4/0.0.0.0/tcp/11235"

  return ()
