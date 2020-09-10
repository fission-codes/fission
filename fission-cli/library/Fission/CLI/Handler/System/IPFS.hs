module Fission.CLI.Handler.System.IPFS
  ( placeBinary
  , placeBinary'
  ) where

import qualified RIO.ByteString.Lazy          as Lazy
import qualified RIO.File                     as File

import           Network.IPFS
import qualified Network.IPFS.File.Types      as File
import           Servant.Client

import           Fission.Prelude

import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Environment.IPFS as Global.IPFS
import qualified Fission.CLI.Environment.OS   as OS
import qualified Fission.CLI.Environment.Path as Path

placeBinary ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadRemoteIPFS  m
  , MonadRescue      m
  , m `Raises` OS.Unsupported
  , m `Raises` ClientError
  )
  => Maybe OS.Supported
  -> m ()
placeBinary (Just os) = placeBinary' os
 placeBinary Nothing   = placeBinary' =<< ensure OS.get

placeBinary' ::
  ( MonadIO          m
  , MonadEnvironment m
  , MonadRemoteIPFS  m
  , MonadRescue      m
  , m `Raises` ClientError
  )
  => OS.Supported
  -> m ()
placeBinary' host = do
  File.Serialized lazyFile <- ensureM . ipfsCat $ Global.IPFS.binCidFor host
  ipfsPath                 <- Path.globalIPFS

  ipfsPath `File.writeBinaryFileDurableAtomic` Lazy.toStrict lazyFile
