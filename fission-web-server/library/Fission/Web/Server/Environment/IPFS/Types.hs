-- | App configuration for IPFS
module Fission.Web.Server.Environment.IPFS.Types (Environment (..)) where

import qualified Network.IPFS.Types as IPFS

import           Fission.Prelude

data Environment = Environment
  { url         :: IPFS.URL           -- ^ IPFS client URL (may be remote)
  , timeout     :: IPFS.Timeout       -- ^ IPFS timeout in seconds
  , binPath     :: IPFS.BinPath       -- ^ Path to local IPFS binary
  , gateway     :: IPFS.Gateway       -- ^ Domain Name of IPFS Gateway
  , remotePeers :: NonEmpty IPFS.Peer -- ^ Remote Peer to connect to
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "IPFS.Environment" \obj -> do
    timeout     <- obj .:? "timeout" .!= 3600
    binPath     <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    gateway     <- obj .:? "gateway" .!= "ipfs.runfission.com"
    url         <- obj .:  "url" >>= parseJSON . String
    remotePeers <- obj .:  "remotePeers"

    return Environment {..}
