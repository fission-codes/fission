-- | General configuration required to run any CLI function
module Fission.CLI.Base.Types (Config (..)) where

import           Control.Concurrent.MVar

import qualified Network.HTTP.Client     as HTTP
import           Network.IPFS.Types      as IPFS
import           Servant.Client

import           Fission.Prelude

import           Fission.User.DID.Types

-- | The configuration used for the CLI application
data Config = Config
  { httpManager   :: !HTTP.Manager
  , fissionURL    :: !BaseUrl
  , ipfsURL       :: !IPFS.URL
  , serverDID     :: !DID
  , logFunc       :: !LogFunc
  , processCtx    :: !ProcessContext
  , ipfsDaemonVar :: !(MVar (Process () () ()))
  }
  deriving Generic

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
