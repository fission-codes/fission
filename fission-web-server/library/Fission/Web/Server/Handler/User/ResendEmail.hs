module Fission.Web.Server.Handler.User.ResendEmail (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Email.Resend.Types      as API.Email

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Challenge.Retriever.Class as Challenge
import           Fission.Web.Server.Email.Class
import           Fission.Web.Server.Email.Types
import qualified Fission.Web.Server.Error                     as Web.Err
import           Fission.Web.Server.Models


handler ::
  ( MonadThrow          m
  , MonadLogger         m
  , MonadEmail          m
  , Challenge.Retriever m
  )
  => ServerT API.Email.Resend m

handler Authorization { about = Entity _ User { userEmail = Nothing } } =
  Web.Err.throw err422 { errBody = "There is no email associated with the user" }

handler Authorization { about = Entity userId User { userUsername = username, userEmail = Just email } } = do
  challenge <- Web.Err.ensureM $ Challenge.retrieve userId

  sendVerificationEmail (Recipient email username) challenge >>= \case
    Left _ ->
      Web.Err.throw err500 { errBody = "Could not send verification email" }
    Right _ ->
      return NoContent
