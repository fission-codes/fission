module Fission.Web.Server.Handler.User.Challenge (handler) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.Challenge.Types               as Challenge

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Challenge.Retriever.Class       as Challenge
import qualified Fission.Web.Server.Challenge.Verifier.Class        as Challenge
import qualified Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge.Creator.Class as RecoveryChallenge
import           Fission.Web.Server.Redirect
import           Fission.Web.Server.User.Retriever.Class            as User

import           Fission.Web.Server.Email.Types
import           Fission.Web.Server.Email.Types

handler ::
  ( Challenge.Retriever       m
  , Challenge.Verifier        m
  , RecoveryChallenge.Creator m
  , User.Retriever            m
  , MonadThrow                m
  , MonadLogger               m
  -- , MonadEmail                m
  , MonadTime                 m
  )
  => Challenge.Routes (AsServerT m)
handler = Challenge.Routes {..}
  where
    recover username = do
      Entity userId User { userEmail } <- Web.Err.ensureMaybe couldntFindUser =<< getByUsername username
      email <- Web.Err.ensureMaybe noAssociatedEmail userEmail
      now       <- currentTime
      challenge <- RecoveryChallenge.create userId now
      return NoContent
      -- return challenge

      where
        couldntFindUser =
          err422 { errBody = "Couldn't find a user with this username" }

        noAssociatedEmail =
          err422 { errBody = "There is no email associated with the user" }
