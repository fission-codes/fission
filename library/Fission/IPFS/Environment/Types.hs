-- | App configuration for IPFS
module Fission.IPFS.Environment.Types
  ( Environment (..)
  , binPath
  , timeout
  , url
  ) where

import RIO hiding (timeout)

import Control.Lens (makeLenses)
import Data.Aeson
import qualified Servant.Client      as Client

-- import qualified Fission.IPFS.Types as IPFS
import           Fission.Internal.Orphanage.PGConnectInfo ()

data Environment = Environment
  { _url     :: !Client.BaseUrl -- ^ IPFS client URL (may be remote)
  , _timeout :: !Natural        -- ^ IPFS timeout in seconds
  , _binPath :: !FilePath        -- ^ Path to local IPFS binary
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "IPFS.Environment" \obj -> do
    _timeout <- obj .:? "timeout" .!= 3600
    _binPath <- obj .:? "binPath" .!= "/usr/local/bin/ipfs"
    _url     <- obj .:  "url" -- >>= parseJSON . String

    return $ Environment {..}
