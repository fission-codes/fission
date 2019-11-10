-- | Web app config
module Fission.Web.Environment.Types (Environment (..)) where

import           Fission.Prelude
import qualified Fission.Web.Types as Web

-- | Configuration for the web application
data Environment = Environment
  { host    :: !Web.Host -- ^ Web app's host
  , port    :: !Web.Port -- ^ Web app's port
  , isTLS   :: !Bool     -- ^ Run over TLS
  , pretty  :: !Bool     -- ^ Pretty-print requests
  , monitor :: !Bool     -- ^ Live monitor application
  } deriving Show

instance FromJSON Environment where
  parseJSON = withObject "Web Config" \obj -> do
    monitor <- obj .:? "monitor" .!= False
    pretty  <- obj .:? "pretty"  .!= False
    isTLS   <- obj .:? "tls"     .!= True
    port    <- obj .:? "port"    .!= Web.Port if isTLS then 443 else 80
    host    <- obj .:  "host"

    return <| Environment {..}
