module Fission.Web.Server.RecoveryChallenge.Retriever.Class (Retriever (..), expiryTime) where

import           Database.Persist

import           Fission.Prelude

import           Fission.Challenge.Types
import           Fission.Error                    as Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Retriever m where
  retrieve :: UserId -> UTCTime -> m (Either (NotFound UserRecoveryChallenge) Challenge)

instance MonadIO m => Retriever (Transaction m) where
  retrieve userId now = do
    res <- selectFirst
      [ UserRecoveryChallengeUserId ==. userId ]
      [ Desc UserRecoveryChallengeInsertedAt  ]
    case res of
      Nothing ->
        return $ Left NotFound

      Just (Entity _ UserRecoveryChallenge { userRecoveryChallengeHash, userRecoveryChallengeUserId, userRecoveryChallengeInsertedAt }) -> do
        let expiryUTCTime = addUTCTime expiryTime userRecoveryChallengeInsertedAt
        if userId == userRecoveryChallengeUserId && expiryUTCTime `isLaterThan` now
          then do
            return $ Right userRecoveryChallengeHash

          else do
            return $ Left NotFound


{-| The time that a challenge is valid for.
If the challenge creation time is longer ago than this time, it's considered expired.

It's 1 hour.
-}
expiryTime :: NominalDiffTime
expiryTime =
  60 -- seconds
  * 60 -- minutes


isLaterThan :: UTCTime -> UTCTime -> Bool
isLaterThan after before =
  nominalDiffTimeToSeconds (after `diffUTCTime` before) > 0
