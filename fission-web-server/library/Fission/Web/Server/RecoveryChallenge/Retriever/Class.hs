module Fission.Web.Server.RecoveryChallenge.Retriever.Class (Retriever (..)) where

import           Database.Persist

import           Fission.Prelude

import           Fission.Challenge.Types
import           Fission.Error                    as Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Retriever m where
  retrieve :: UserId -> m (Either (NotFound UserRecoveryChallenge) Challenge)

instance MonadIO m => Retriever (Transaction m) where
  retrieve userId = do
    res <- selectFirst [ UserRecoveryChallengeUserId ==. userId ] []
    case res of
      Nothing ->
        return $ Left NotFound

      Just (Entity _ UserRecoveryChallenge { userRecoveryChallengeHash, userRecoveryChallengeUserId }) ->
        if userId == userRecoveryChallengeUserId
          then do
            return $ Right userRecoveryChallengeHash

          else do
            return $ Left NotFound
