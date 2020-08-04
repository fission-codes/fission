module Fission.CLI.Connected.Types (Config (..)) where

import qualified Crypto.PubKey.Ed25519  as Ed25519
import           Network.HTTP.Client    as HTTP
import           Network.IPFS.Types     as IPFS
import           Servant.Client

import           Fission.Prelude        hiding (mask, uninterruptibleMask)
import           Fission.User.DID.Types

data Config = Config
  { httpManager  :: !HTTP.Manager
  , secretKey    :: !Ed25519.SecretKey
  , cliDID       :: !DID
  , serverDID    :: !DID
  -- TODO link systems, ucanLink     :: !JWT
  , fissionURL   :: !BaseUrl
  , logFunc      :: !LogFunc
  , processCtx   :: !ProcessContext
  , ipfsPath     :: !IPFS.BinPath
  , ipfsTimeout  :: !IPFS.Timeout
  , peers        :: !(NonEmpty IPFS.Peer)
  , ignoredFiles :: !IPFS.Ignored
  }
  deriving (Generic)

instance HasProcessContext Config where
  processContextL = lens processCtx \cfg newProcessCtx ->
    cfg { processCtx = newProcessCtx }

instance HasLogFunc Config where
  logFuncL = lens logFunc \cfg newLogFunc' ->
    cfg { logFunc = newLogFunc' }
