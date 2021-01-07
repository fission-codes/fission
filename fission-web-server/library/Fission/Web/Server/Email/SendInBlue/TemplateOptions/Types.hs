module Fission.Web.Server.Email.SendInBlue.TemplateOptions.Types (TemplateOptions(..)) where

import           Servant.Client

import           Fission.Prelude

import           Fission.User.Username.Types

data TemplateOptions = TemplateOptions
  { verifyLink :: BaseUrl
  , username   :: Username
  }

instance ToJSON TemplateOptions where
  toJSON TemplateOptions { verifyLink, username } =
    Object [ ("VERIFY_LINK", toJSON verifyLink )
           , ("USERNAME",    toJSON username )
           ]
