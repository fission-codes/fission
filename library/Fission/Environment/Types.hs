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

import qualified Fission.IPFS.Environment.Types    as IPFS
import qualified Fission.Storage.Environment.Types as Storage
import qualified Fission.Web.Environment.Types     as Web

data Environment = Environment
  { _ipfs    :: !IPFS.Environment    -- ^ IPFS configuration
  , _storage :: !Storage.Environment -- ^ Storage/DB configuration
  , _web     :: !Web.Environment     -- ^ Web configuration
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    _ipfs    <- parseJSON . Object =<< obj .: "ipfs"
    _storage <- parseJSON . Object =<< obj .: "storage"
    _web     <- parseJSON . Object =<< obj .: "web"

    return $ Environment {..}
