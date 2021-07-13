module Fission.Web.Server.Handler.User.Email.Recover (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.Email.Recover.Types           as API.Email

import           Fission.Web.Server.Email.Class
import           Fission.Web.Server.Email.Types
import qualified Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge.Creator.Class as RecoveryChallenge
import           Fission.Web.Server.User.Retriever.Class            as User


handler ::
  ( MonadThrow                m
  , MonadLogger               m
  , MonadEmail                m
  , MonadTime                 m
  , RecoveryChallenge.Creator m
  , User.Retriever            m
  )
  => ServerT API.Email.Recover m

handler username = do
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
