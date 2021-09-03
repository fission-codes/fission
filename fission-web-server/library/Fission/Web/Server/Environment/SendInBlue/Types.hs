-- | Configuration for Send In Blue API Requests
module Fission.Web.Server.Environment.SendInBlue.Types (Environment (..)) where

import           Fission.Prelude

import qualified Fission.Web.API.Host.Types                as Web

import qualified Fission.Web.Server.Email.SendInBlue.Types as SIB

data Environment = Environment
  { sibApiKey                      :: SIB.ApiKey     -- ^ API Key for SendInBlue
  , sibUrl                         :: Web.Host       -- ^ Base url for API
  , sibVerificationEmailTemplateId :: SIB.TemplateId -- ^ Verification email template ID
  , sibRecoveryEmailTemplateId     :: SIB.TemplateId -- ^ Recovery email template ID
  , sibRecoveryAppUrl              :: Text           -- ^ Recovery app url to link in recovery emails
  } deriving (Show, Eq)

instance FromJSON Environment where
  parseJSON = withObject "SendInBlue.Environment" \obj -> do
    sibApiKey                      <- obj .: "api_key"
    sibUrl                         <- obj .: "base_url"
    sibVerificationEmailTemplateId <- obj .: "verification_email_template_id"
    sibRecoveryEmailTemplateId     <- obj .: "recovery_email_template_id"
    sibRecoveryAppUrl              <- obj .: "recovery_app_url"

    return Environment {..}
