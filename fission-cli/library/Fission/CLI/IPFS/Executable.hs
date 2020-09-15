module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy          as Lazy

import           Network.IPFS
import qualified Network.IPFS.BinPath.Types   as IPFS
import qualified Network.IPFS.File.Types      as File
import           Servant.Client

import           Fission.Prelude

import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Environment.IPFS as Global.IPFS
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

  File.Serialized lazyFile <- ensureM . ipfsCat $ Global.IPFS.binCidFor host
  IPFS.BinPath ipfsPath    <- Path.globalIPFS

  logDebug ipfsPath

  ipfsPath `forceWrite` Lazy.toStrict lazyFile
