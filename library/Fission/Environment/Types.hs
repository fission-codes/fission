-- | External app configuration ("knobs")
module Fission.Environment.Types (Environment (..)) where

import Fission.Prelude

import qualified Fission.IPFS.Environment.Types    as IPFS
import qualified Fission.Storage.Environment.Types as Storage
import qualified Fission.Web.Environment.Types     as Web
import qualified Fission.AWS.Environment.Types     as AWS

-- | Top-level application configuration. The "knobs" for your app.
data Environment = Environment
  { ipfs    :: !IPFS.Environment    -- ^ IPFS configuration
  , storage :: !Storage.Environment -- ^ Storage/DB configuration
  , web     :: !Web.Environment     -- ^ Web configuration
  , aws     :: !AWS.Environment     -- ^ AWS configuration
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    ipfs    <- obj .: "ipfs"
    storage <- obj .: "storage"
    web     <- obj .: "web"
    aws     <- obj .: "aws"

    return <| Environment {..}
