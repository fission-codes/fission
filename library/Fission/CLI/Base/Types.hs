-- | General configuration required to run any CLI function
module Fission.CLI.Base.Types (Config (..)) where

import qualified Network.HTTP.Client    as HTTP
import           Servant.Client

import           Fission.Prelude        hiding (mask, uninterruptibleMask)

import           Fission.User.DID.Types

-- | The configuration used for the CLI application
data Config = Config
  { httpManager     :: !HTTP.Manager
  , fissionURL      :: !BaseUrl
  , cachedServerDID :: !(Maybe DID) -- ^ Typically from setting with envar
  , logFunc         :: !LogFunc
  , processCtx      :: !ProcessContext
  }
  deriving Generic

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
