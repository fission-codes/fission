module Fission.CLI.IPFS.Executable
  ( place
  , place'
  ) where

import qualified RIO.ByteString.Lazy          as Lazy
import qualified RIO.Text                     as Text

import qualified Turtle                       as Turtle

import           Network.HTTP.Client          as HTTP
import           Network.IPFS
import qualified Network.IPFS.File.Types      as File
import           Network.IPFS.Types           as IPFS
import           Servant.Client

import           Fission.Prelude

import qualified Fission.CLI.Environment.IPFS as IPFS

import           Fission.CLI.Bootstrap
import           Fission.CLI.Environment      as Env
import qualified Fission.CLI.Environment.OS   as OS
import qualified Fission.CLI.Environment.Path as Path
import           Fission.CLI.File

place ::
  ( MonadIO          m
  , MonadLogger      m
  , MonadEnvironment m
  -- FIXME deep annoyance
  , MonadReader cfg m
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "ipfsURL"     cfg IPFS.URL
  --
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
  -- FIXME deep annoyance
  , MonadReader cfg m
  , HasField' "httpManager" cfg HTTP.Manager
  , HasField' "ipfsURL"     cfg IPFS.URL
  --
  , MonadRescue      m
  , m `Raises` ClientError
  )
  => OS.Supported
  -> m ()
place' host = do
  logDebug $ "Setting up IPFS binary for " <> textDisplay host

  IPFS.BinPath    ipfsPath <- Path.globalIPFS
  File.Serialized lazyFile <- ensureM $ runBootstrapT do
    ipfsCat $ IPFS.binCidFor host

  logDebug $ "Writing IPFS binary to " <> Text.pack ipfsPath
  ipfsPath `forceWrite` Lazy.toStrict lazyFile
  void . Turtle.chmod Turtle.executable $ Turtle.decodeString ipfsPath
