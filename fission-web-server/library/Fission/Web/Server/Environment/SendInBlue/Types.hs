-- | Configuration for Send In Blue API Requests
module Fission.Web.Server.Environment.SendInBlue.Types (Environment (..)) where

import           Fission.Prelude

import qualified Fission.Web.Server.Email.SendInBlue.Types as SIB
import qualified Fission.Web.Server.Host.Types             as Web

data Environment = Environment
  { sibApiKey                      :: SIB.ApiKey     -- ^ API Key for SendInBlue
  , sibUrl                         :: Web.Host       -- ^ Base url for API
  , sibVerificationEmailTemplateId :: SIB.TemplateId -- ^ Verification email template ID
  , sibRecoveryEmailTemplateId     :: SIB.TemplateId -- ^ Recovery email template ID
  } deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "SendInBlue.Environment" \obj -> do
    sibApiKey                      <- obj .: "api_key"
    sibUrl                         <- obj .: "base_url"
    sibVerificationEmailTemplateId <- obj .: "verification_email_template_id"
    sibRecoveryEmailTemplateId     <- obj .: "recovery_email_template_id"

    return Environment {..}
