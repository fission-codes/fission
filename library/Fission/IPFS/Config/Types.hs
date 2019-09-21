-- | App configuration for IPFS
module Fission.IPFS.Config.Types (Config (..)) where

import RIO hiding (timeout)

import Data.Aeson

import qualified Fission.IPFS.Types as IPFS

import Fission.Internal.Orphanage.PGConnectInfo ()

data Config = Config
  { url     :: !IPFS.URL
  , timeout :: !IPFS.Timeout
  , binPath :: !IPFS.BinPath
  } deriving Show

instance FromJSON Config where
  parseJSON = withObject "IPFS Config" \obj -> do
    url     <- obj .:  "url"
    timeout <- obj .:  "timeout" .!= 3600
    binPath <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    return $ Config {..}
