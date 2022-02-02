module Fission.Web.Server.Environment.PowerDNS.Types (Environment (..)) where

import           Fission.Prelude
import qualified Fission.Web.Server.PowerDNS.Types as PowerDNS

data Environment
  = Environment
    { apiURL :: PowerDNS.URL
    , apiKey :: PowerDNS.ApiKey
    }
  deriving Eq

instance Show Environment where
  show Environment {..} = intercalate "\n"
    [ "Environment {"
    , "  url     = " <> show apiURL
    , "  api_key = HIDDEN"
    ]

instance FromJSON Environment where
  parseJSON = withObject "PowerDNS.Environment" \obj -> do
    apiURL    <- obj .: "api_url"
    apiKey <- obj .: "api_key"

    return Environment {..}
