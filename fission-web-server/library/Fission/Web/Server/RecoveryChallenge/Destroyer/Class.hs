module Fission.Web.Server.RecoveryChallenge.Destroyer.Class (Destroyer (..)) where

import           Database.Esqueleto.Legacy

import           Fission.Prelude

import           Fission.Web.Server.Models        as Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Destroyer m where
  destroyForUser :: UserId -> m ()

instance MonadIO m => Destroyer (Transaction m) where
  destroyForUser userId =
    delete $ from \challenge ->
      where_ $ challenge ^. UserRecoveryChallengeUserId ==. val userId
