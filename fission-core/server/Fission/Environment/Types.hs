-- | External app configuration ("knobs")
module Fission.Environment.Types (Environment (..)) where

import Fission.Prelude

import qualified Fission.Environment.Auth.Types       as Auth
import qualified Fission.Environment.AWS.Types        as AWS
import qualified Fission.Environment.FFS.Types        as FFS
import qualified Fission.Environment.IPFS.Types       as IPFS
import qualified Fission.Environment.Storage.Types    as Storage
import qualified Fission.Environment.Server.Types     as Server
import qualified Fission.Environment.WebApp.Types     as WebApp
import qualified Fission.Environment.SendInBlue.Types as SendInBlue

-- | Top-level application configuration. The "knobs" for your app.
data Environment = Environment
  { auth       :: !Auth.Environment        -- ^ Auth & ID config
  , aws        :: !AWS.Environment         -- ^ AWS configuration
  , ffs        :: !FFS.Environment         -- ^ Fission File System configuration
  , ipfs       :: !IPFS.Environment        -- ^ IPFS configuration
  , storage    :: !Storage.Environment     -- ^ Storage/DB configuration
  , server     :: !Server.Environment      -- ^ Server configuration
  , webApp     :: !WebApp.Environment      -- ^ WebApp configuration
  , sendInBlue :: !SendInBlue.Environment  -- ^ WebApp configuration
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Environment" \obj -> do
    auth       <- obj .: "auth"
    aws        <- obj .: "aws"
    ffs        <- obj .: "fission_file_system"
    ipfs       <- obj .: "ipfs"
    storage    <- obj .: "storage"
    server     <- obj .: "web"
    webApp     <- obj .: "web_app"
    sendInBlue <- obj .: "send_in_blue"

    return Environment {..}
