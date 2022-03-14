-- | General configuration required to run any CLI function
module Fission.CLI.Base.Types (Config (..)) where

import           Control.Concurrent.MVar

import qualified Network.HTTP.Client     as HTTP
import           Network.IPFS.Types      as IPFS

import           Fission.Prelude

import           Web.DID.Types

import           Fission.CLI.Remote

-- | The configuration used for the CLI application
data Config = Config
  { httpManager   :: HTTP.Manager
  , tlsManager    :: HTTP.Manager
  , ipfsTimeout   :: IPFS.Timeout
  , ipfsURL       :: IPFS.URL
  , remote        :: Remote
  , serverDID     :: DID
  , logFunc       :: LogFunc
  , processCtx    :: ProcessContext
  , ipfsDaemonVar :: MVar (Process () () ())
  }
  deriving Generic

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
