-- | App configuration for IPFS
module Fission.Environment.IPFS.Types (Environment (..)) where

import           Fission.Prelude
import qualified Network.IPFS.Types as IPFS

data Environment = Environment
  { url        :: !IPFS.URL     -- ^ IPFS client URL (may be remote)
  , timeout    :: !IPFS.Timeout -- ^ IPFS timeout in seconds
  , binPath    :: !IPFS.BinPath -- ^ Path to local IPFS binary
  , gateway    :: !IPFS.Gateway -- ^ Domain Name of IPFS Gateway
  , remotePeer :: !IPFS.Peer    -- ^ Remote Peer to connect to
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "IPFS.Environment" \obj -> do
    timeout    <- obj .:? "timeout" .!= 3600
    binPath    <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    gateway    <- obj .:? "gateway" .!= "ipfs.runfission.com"
    url        <- obj .:  "url" >>= parseJSON . String
    remotePeer <- obj .:  "remotePeer"

    return Environment {..}
