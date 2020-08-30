module Fission.Email.SendInBlue.Client 
  ( SendEmailAPI
  , sendEmail
  ) where

import Fission.Prelude
import Fission.Email.SendInBlue.Types as Email

import Servant
import Servant.Client


type SendEmailAPI
  = "v3"
  :> "smtp"
  :> "email"
  :> Header "api-key" Text
  :> ReqBody '[JSON] Email.Request
  :> Post    '[JSON] Email.Response

sendEmail :: ApiKey -> Email.Request -> ClientM Email.Response
sendEmail (ApiKey key) = (client $ Proxy @SendEmailAPI) (Just key)
