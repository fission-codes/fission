module Fission.Web.Server.Email.SendInBlue.VerificationTemplateOptions.Types (VerificationTemplateOptions(..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.User.Username.Types

data VerificationTemplateOptions = VerificationTemplateOptions
  { verifyLink :: BaseUrl
  , username   :: Username
  }

instance ToJSON VerificationTemplateOptions where
  toJSON VerificationTemplateOptions { verifyLink, username } =
    Object [ ("VERIFY_LINK", toJSON verifyLink )
           , ("USERNAME",    toJSON username )
           ]
