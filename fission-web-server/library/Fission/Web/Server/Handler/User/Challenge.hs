module Fission.Web.Server.Handler.User.Challenge (handler) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.Challenge.Types               as Challenge

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error                           as Web.Err
import           Fission.Web.Server.Models
import qualified Fission.Web.Server.RecoveryChallenge.Creator.Class as RecoveryChallenge
import           Fission.Web.Server.User.Retriever.Class            as User

handler ::
  ( RecoveryChallenge.Creator m
  , User.Retriever            m
  , MonadThrow                m
  , MonadLogger               m
  , MonadTime                 m
  )
  => Challenge.Routes (AsServerT m)
handler = Challenge.Routes {..}
  where
    recover username Authorization { about = Entity userId _ } = do
      Entity _ User { } <- Web.Err.ensureMaybe couldntFindUser =<< getByUsername username
      now       <- currentTime
      challenge <- RecoveryChallenge.create userId now
      return challenge

      where
        couldntFindUser =
          err422 { errBody = "Couldn't find a user with this username" }
