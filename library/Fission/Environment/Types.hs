-- | External app configuration ("knobs")
module Fission.Environment.Types
  ( Environment (..)
  , ipfs
  , storage
  , web
  ) where

import RIO hiding (timeout)

import Data.Aeson
import Control.Lens (makeLenses)

import qualified Fission.IPFS.Config.Types    as IPFS
import qualified Fission.Storage.Config.Types as Storage
import qualified Fission.Web.Config.Types     as Web

data Environment = Environment
  { _ipfs    :: !IPFS.Config    -- ^ IPFS configuration
  , _storage :: !Storage.Config -- ^ Storage/DB configuration
  , _web     :: !Web.Config     -- ^ Web configuration
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    _ipfs    <- parseJSON . Object =<< obj .: "ipfs"
    _storage <- parseJSON . Object =<< obj .: "storage"
    _web     <- parseJSON . Object =<< obj .: "web"

    return $ Environment {..}
