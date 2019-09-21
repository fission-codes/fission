-- | Web app config
module Fission.Web.Config.Types (Config (..)) where

import RIO

import Data.Aeson

import qualified Fission.Web.Types as Web

-- | Configuration for the web application
data Config = Config
  { host  :: !Web.Host
  , post  :: !Web.Port
  , isTLS :: !Bool
  }

instance FromJSON Config where
  parseJSON = withObject "Web Config" \obj -> do
    isTLS <- obj .:? "tls"  .!= True
    post  <- obj .:? "port" .!= Web.Port if isTLS then 443 else 80
    host  <- obj .:  "host"
    return $ Config {..}
