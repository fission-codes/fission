module Fission.Web.Server.Handler.User.Email (handler) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.Email.Types                   as Email

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Challenge.Retriever.Class       as Challenge
import qualified Fission.Web.Server.Challenge.Verifier.Class        as Challenge
import qualified Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge.Creator.Class as RecoveryChallenge
import           Fission.Web.Server.Redirect
import           Fission.Web.Server.User.Retriever.Class            as User

import           Fission.Web.Server.Email.Class
import           Fission.Web.Server.Email.Types

handler ::
  ( Challenge.Retriever       m
  , Challenge.Verifier        m
  , RecoveryChallenge.Creator m
  , User.Retriever            m
  , MonadThrow                m
  , MonadLogger               m
  , MonadEmail                m
  , MonadTime                 m
  )
  => Email.Routes (AsServerT m)
handler = Email.Routes {..}
  where
    verify challenge =
      Challenge.verify challenge >>= \case
        Left  _ -> Web.Err.throw err404 { errBody = "User does not exist" }
        Right _ -> redirect "https://fission.codes?verified=true"

    resend Authorization { about = Entity userId User { userUsername = username, userEmail } } =
      case userEmail of
        Nothing ->
          Web.Err.throw err422 { errBody = "There is no email associated with the user" }

        Just email -> do
          challenge <- Web.Err.ensureM $ Challenge.retrieve userId
          Web.Err.ensureM $ sendVerificationEmail (Recipient email username) challenge
          return NoContent

    recover username = do
      Entity userId User { userEmail } <- Web.Err.ensureMaybe couldntFindUser =<< getByUsername username
      email <- Web.Err.ensureMaybe noAssociatedEmail userEmail
      now       <- currentTime
      challenge <- RecoveryChallenge.create userId now
      Web.Err.ensureM $ sendRecoveryEmail (Recipient email username) challenge
      return NoContent

      where
        couldntFindUser =
          err422 { errBody = "Couldn't find a user with this username" }

        noAssociatedEmail =
          err422 { errBody = "There is no email associated with the user" }
