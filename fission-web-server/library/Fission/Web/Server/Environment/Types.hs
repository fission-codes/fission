-- | External app configuration ("knobs")
module Fission.Web.Server.Environment.Types (Environment (..)) where

import           Fission.Prelude

import qualified Fission.Web.Server.Environment.Auth.Types       as Auth
import qualified Fission.Web.Server.Environment.AWS.Types        as AWS
import qualified Fission.Web.Server.Environment.FFS.Types        as FFS
import qualified Fission.Web.Server.Environment.IPFS.Types       as IPFS
import qualified Fission.Web.Server.Environment.SendInBlue.Types as SendInBlue
import qualified Fission.Web.Server.Environment.Server.Types     as Server
import qualified Fission.Web.Server.Environment.Storage.Types    as Storage
import qualified Fission.Web.Server.Environment.WebApp.Types     as WebApp

-- | Top-level application configuration. The "knobs" for your app.
data Environment = Environment
  { auth       :: Auth.Environment        -- ^ Auth & ID config
  , aws        :: AWS.Environment         -- ^ AWS configuration
  , ffs        :: FFS.Environment         -- ^ Fission File System configuration
  , ipfs       :: IPFS.Environment        -- ^ IPFS configuration
  , storage    :: Storage.Environment     -- ^ Storage/DB configuration
  , server     :: Server.Environment      -- ^ Server configuration
  , webApp     :: WebApp.Environment      -- ^ WebApp configuration
  , sendInBlue :: SendInBlue.Environment  -- ^ WebApp configuration
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
