-- | App configuration for IPFS
module Fission.Web.Server.Environment.IPFS.Types (Environment (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

data Environment = Environment
  { timeout     :: IPFS.Timeout       -- ^ IPFS timeout in seconds
  , gateway     :: IPFS.Gateway       -- ^ Domain Name of IPFS Gateway
  , binPath     :: IPFS.BinPath       -- ^ Path to local IPFS binary
  , urls        :: NonEmpty IPFS.URL  -- ^ IPFS client URLs (may be remote)
  , remotePeers :: NonEmpty IPFS.Peer -- ^ Remote Peers for users to connect to
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "IPFS.Environment" \obj -> do
    timeout     <- obj .:? "timeout" .!= 3600
    binPath     <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    gateway     <- obj .:? "gateway" .!= "ipfs.runfission.com"
    urls        <- obj .:  "urls"
    remotePeers <- obj .:  "remotePeers"

    return Environment {..}
