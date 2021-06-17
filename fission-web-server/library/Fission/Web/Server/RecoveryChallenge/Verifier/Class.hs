module Fission.Web.Server.RecoveryChallenge.Verifier.Class (Verifier (..)) where

import           Database.Persist

import           Fission.Prelude

import           Fission.Challenge.Types
import           Fission.Error                    as Error

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Verifier m where
  verify :: Challenge -> m (Either (NotFound UserChallenge) ())

instance MonadIO m => Verifier (Transaction m) where
  verify challenge = do
    res <- selectFirst [ UserChallengeHash ==. challenge ] []
    case res of
      Nothing ->
        return $ Left NotFound

      Just (Entity _ UserChallenge { userChallengeHash, userChallengeUserId }) ->
        if challenge == userChallengeHash
          then do
            update userChallengeUserId [ UserVerified  =. True ]
            return ok

        else
            return $ Left NotFound
