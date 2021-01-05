module Fission.Web.Server.Email.SendInBlue.Client
  ( SendEmailAPI
  , sendEmail
  ) where

import           Servant
import           Servant.Client

import           Fission.Prelude
import           Fission.Web.Server.Email.SendInBlue.Types as Email

type SendEmailAPI
  = "v3"
  :> "smtp"
  :> "email"
  :> Header "api-key" Text
  :> ReqBody '[JSON] Email.Request
  :> Post    '[JSON] Email.Response

sendEmail :: ApiKey -> Email.Request -> ClientM Email.Response
sendEmail (ApiKey key) = (client $ Proxy @SendEmailAPI) (Just key)
