-- | Configuration for Send In Blue API Requests
module Fission.Environment.SendInBlue.Types (Environment (..)) where

import           Fission.Prelude
import qualified Fission.Web.Types              as Web
import qualified Fission.Email.SendInBlue.Types as SIB

data Environment = Environment
  { sibApiKey     :: !SIB.ApiKey     -- ^ API Key for SendInBlue
  , sibUrl        :: !Web.Host       -- ^ Base url for API
  , sibTemplateId :: !SIB.TemplateId -- ^ Email template ID
  } deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "SendInBlue.Environment" \obj -> do
    sibApiKey     <- obj .: "api_key"
    sibUrl        <- obj .: "base_url"
    sibTemplateId <- obj .: "template_id"

    return Environment {..}
