-- | Server config
module Fission.Web.Server.Environment.Server.Types (Environment (..)) where

import           Fission.Prelude

import           Fission.Web.API.Remote              (Remote)

import qualified Fission.Web.Server.AWS.Types        as AWS
import qualified Fission.Web.Server.Host.Types       as Web
import qualified Fission.Web.Server.Sentry.DSN.Types as Sentry

-- | Configuration for the web application
data Environment = Environment
  { host         :: Web.Host         -- ^ Web app's host
  , environment  :: Remote           -- ^ Which remote this server is acting as / env
  , port         :: Web.Port         -- ^ Web app's port
  , isTLS        :: Bool             -- ^ Run over TLS
  , useEKG       :: Bool             -- ^ Run with EKG
  , pretty       :: Bool             -- ^ Pretty-print requests
  , sentryDSN    :: Maybe Sentry.DSN -- ^ Sentry DSN key
  , serverZoneID :: AWS.ZoneID       -- ^ Hosted Zone of this server (runfission.com at time of writing)
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Web Config" \obj -> do
    sentryDSN    <- obj .:? "sentry_dsn"
    pretty       <- obj .:? "pretty" .!= False
    isTLS        <- obj .:? "tls"    .!= True
    useEKG       <- obj .:? "ekg"    .!= True
    port         <- obj .:? "port"   .!= Web.Port if isTLS then 443 else 80
    host         <- obj .:  "host"
    environment  <- obj .:  "environment"
    serverZoneID <- obj .:  "zone_id"

    return Environment {..}
