-- | Web app config
module Fission.Web.Config.Types
  ( Config (..)
  , host
  , isTLS
  , monitor
  , port
  , pretty
  ) where

import RIO

import Control.Lens (makeLenses)
import Data.Aeson

import qualified Fission.Web.Types as Web

-- | Configuration for the web application
data Config = Config
  { _host    :: !Web.Host -- ^ Web app's host
  , _port    :: !Web.Port -- ^ Web app's port
  , _isTLS   :: !Bool     -- ^ Run over TLS
  , _pretty  :: !Bool     -- ^ Pretty-print requests
  , _monitor :: !Bool     -- ^ Live monitor application
  } deriving Show

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Web Config" \obj -> do
    _monitor <- obj .:? "monitor" .!= False
    _pretty  <- obj .:? "pretty"  .!= False
    _isTLS   <- obj .:? "tls"     .!= True
    _port    <- obj .:? "port"    .!= Web.Port if _isTLS then 443 else 80
    _host    <- obj .:  "host"

    return $ Config {..}
