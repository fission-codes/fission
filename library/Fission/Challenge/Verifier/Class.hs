module Fission.Challenge.Verifier.Class (Verifier (..)) where

import           Database.Persist

import           Fission.Prelude
import           Fission.Error as Error
import           Fission.Models
import           Fission.Challenge.Types


class Monad m => Verifier m where
  verify :: Challenge -> m (Either (NotFound UserChallenge) ())

instance MonadIO m => Verifier (Transaction m) where
  verify challenge = do
    res <- selectFirst [ UserChallengeHash ==. challenge ] [] 
    case res of
      Nothing -> 
        return $ Left NotFound

      Just (Entity challengeId UserChallenge { userChallengeHash, userChallengeUserId }) -> 
        if challenge == userChallengeHash
          then do
            update userChallengeUserId
              [ UserVerified  =. True ]

            delete challengeId

            return ok

          else
            return $ Left NotFound
