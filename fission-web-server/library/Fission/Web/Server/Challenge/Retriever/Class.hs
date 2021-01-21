module Fission.Web.Server.Challenge.Retriever.Class (Retriever (..)) where

import           Database.Persist

import           Fission.Prelude

import           Fission.Challenge.Types
import           Fission.Error                    as Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Retriever m where
  retrieve :: UserId -> m (Either (NotFound UserChallenge) Challenge)

instance MonadIO m => Retriever (Transaction m) where
  retrieve userId = do
    res <- selectFirst [ UserChallengeUserId ==. userId ] []
    case res of
      Nothing ->
        return $ Left NotFound

      Just (Entity _ UserChallenge { userChallengeHash, userChallengeUserId }) ->
        if userId == userChallengeUserId
          then do
            return $ Right userChallengeHash

          else do
            return $ Left NotFound
