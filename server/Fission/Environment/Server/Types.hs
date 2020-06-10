-- | Server config
module Fission.Environment.Server.Types (Environment (..)) where

import           Fission.Prelude
import qualified Fission.AWS.Types as AWS
 
import qualified Fission.Web.Types                as Web
import qualified Fission.Web.Log.Sentry.DSN.Types as Sentry

-- | Configuration for the web application
data Environment = Environment
  { host         :: !Web.Host           -- ^ Web app's host
  , port         :: !Web.Port           -- ^ Web app's port
  , isTLS        :: !Bool               -- ^ Run over TLS
  , pretty       :: !Bool               -- ^ Pretty-print requests
  , sentryDSN    :: !(Maybe Sentry.DSN) -- ^ Sentry DSN key
  , serverZoneID :: !AWS.ZoneID         -- ^ Hosted Zone of this server (runfission.com at time of writing)
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Web Config" \obj -> do
    sentryDSN    <- obj .:? "sentry_dsn"
    pretty       <- obj .:? "pretty" .!= False
    isTLS        <- obj .:? "tls"    .!= True
    port         <- obj .:? "port"   .!= Web.Port if isTLS then 443 else 80
    host         <- obj .:  "host"
    serverZoneID <- obj .:  "zone_id"

    return Environment {..}
