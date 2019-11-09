-- | App configuration for IPFS
module Fission.IPFS.Environment.Types (Environment (..)) where

import           Fission.Prelude
import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { url     :: !IPFS.URL     -- ^ IPFS client URL (may be remote)
  , timeout :: !IPFS.Timeout -- ^ IPFS timeout in seconds
  , binPath :: !IPFS.BinPath -- ^ Path to local IPFS binary
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "IPFS.Environment" \obj -> do
    timeout <- obj .:? "timeout" .!= 3600
    binPath <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    url     <- obj .:  "url" >>= parseJSON . String

    return $ Environment {..}
