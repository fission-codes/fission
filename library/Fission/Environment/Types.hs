-- | External app configuration ("knobs")
module Fission.Environment.Types
  ( Environment (..)
  , ipfs
  , storage
  , web
  , aws
  ) where

import RIO hiding (timeout)

import Data.Aeson
import Control.Lens (makeLenses)

import qualified Fission.IPFS.Environment.Types    as IPFS
import qualified Fission.Storage.Environment.Types as Storage
import qualified Fission.Web.Environment.Types     as Web
import qualified Fission.AWS.Environment.Types     as AWS

-- | Top-level application configuration. The "knobs" for your app.
data Environment = Environment
  { _ipfs    :: !IPFS.Environment    -- ^ IPFS configuration
  , _storage :: !Storage.Environment -- ^ Storage/DB configuration
  , _web     :: !Web.Environment     -- ^ Web configuration
  , _aws     :: !AWS.Environment     -- ^ AWS configuration
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    _ipfs    <- obj .: "ipfs"
    _storage <- obj .: "storage"
    _web     <- obj .: "web"
    _aws     <- obj .: "aws"

    return $ Environment {..}
