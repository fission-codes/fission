-- | Web app config
module Fission.Web.Environment.Types
  ( Environment (..)
  , host
  , isTLS
  , monitor
  , port
  , pretty
  , sentryDSN
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.Aeson

import qualified Fission.Web.Types as Web
import qualified Fission.Web.Log.Sentry.Types as Sentry

-- | Configuration for the web application
data Environment = Environment
  { _host      :: !Web.Host           -- ^ Web app's host
  , _port      :: !Web.Port           -- ^ Web app's port
  , _isTLS     :: !Bool               -- ^ Run over TLS
  , _pretty    :: !Bool               -- ^ Pretty-print requests
  , _monitor   :: !Bool               -- ^ Live monitor application
  , _sentryDSN :: !(Maybe Sentry.DSN) -- ^ Sentry DSN key
  } deriving Show

makeLenses ''Environment

instance FromJSON Environment where
  parseJSON = withObject "Web Config" \obj -> do
    _sentryDSN <- obj .:? "sentry_dsn"
    _monitor   <- obj .:? "monitor" .!= False
    _pretty    <- obj .:? "pretty"  .!= False
    _isTLS     <- obj .:? "tls"     .!= True
    _port      <- obj .:? "port"    .!= Web.Port if _isTLS then 443 else 80
    _host      <- obj .:  "host"

    return $ Environment {..}
