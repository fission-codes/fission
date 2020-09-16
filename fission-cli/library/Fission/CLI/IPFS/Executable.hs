module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy          as Lazy
import qualified RIO.Text                     as Text

import           Network.IPFS
import qualified Network.IPFS.BinPath.Types   as IPFS
import qualified Network.IPFS.File.Types      as File
import           Servant.Client

import           Fission.Prelude
import           Turtle                       as Turtle

import qualified Fission.CLI.Environment.IPFS as IPFS

import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Environment.OS   as OS
import qualified Fission.CLI.Environment.Path as Path
import           Fission.CLI.File

place ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadEnvironment m
  , MonadRemoteIPFS  m
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
  , MonadEnvironment m
  , MonadRemoteIPFS  m
  , MonadRescue      m
  , m `Raises` ClientError
  )
  => OS.Supported
  -> m ()
place' host = do
  logDebug $ "Setting up IPFS binary for " <> textDisplay host

  IPFS.BinPath    ipfsPath <- Path.globalIPFS
  File.Serialized lazyFile <- ensureM . ipfsCat $ IPFS.binCidFor host

  logDebug $ "Writing IPFS binary to " <> Text.pack ipfsPath
  ipfsPath `forceWrite` Lazy.toStrict lazyFile
  void . chmod executable $ Turtle.decodeString ipfsPath
