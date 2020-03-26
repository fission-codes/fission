-- | External app configuration ("knobs")
module Fission.Environment.Types (Environment (..)) where

import Fission.Prelude

import qualified Fission.Environment.AWS.Types     as AWS
import qualified Fission.Environment.FFS.Types     as FFS
import qualified Fission.Environment.IPFS.Types    as IPFS
import qualified Fission.Environment.Storage.Types as Storage
import qualified Fission.Environment.Web.Types     as Web
import qualified Fission.Environment.WebApp.Types  as WebApp

-- | Top-level application configuration. The "knobs" for your app.
data Environment = Environment
  { ipfs    :: !IPFS.Environment    -- ^ IPFS configuration
  , storage :: !Storage.Environment -- ^ Storage/DB configuration
  , web     :: !Web.Environment     -- ^ Web configuration
  , aws     :: !AWS.Environment     -- ^ AWS configuration
  , webApp  :: !WebApp.Environment  -- ^ WebApp configuration
  , ffs     :: !FFS.Environment     -- ^ Fission File System configuration
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    ipfs    <- obj .: "ipfs"
    storage <- obj .: "storage"
    web     <- obj .: "web"
    aws     <- obj .: "aws"
    webApp  <- obj .: "web_app"
    ffs     <- obj .: "fission_file_system"

    return <| Environment {..}
