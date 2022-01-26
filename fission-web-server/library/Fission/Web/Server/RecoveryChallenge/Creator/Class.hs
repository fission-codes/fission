module Fission.Web.Server.RecoveryChallenge.Creator.Class (Creator (..)) where

import           Database.Esqueleto.Legacy

import           Fission.Prelude

import           Fission.Challenge.Types
import qualified Fission.Random                   as Random

import           Fission.Web.Server.Models        as Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Creator m where
  create :: UserId -> UTCTime -> m Challenge

instance MonadIO m => Creator (Transaction m) where
  create userId now = do
    challenge <- mkChallenge
    insert UserRecoveryChallenge
      { userRecoveryChallengeUserId     = userId
      , userRecoveryChallengeHash       = challenge
      , userRecoveryChallengeInsertedAt = now
      }
    return challenge

mkChallenge :: MonadIO m => m Challenge
mkChallenge = Challenge <$> Random.alphaNum 32
