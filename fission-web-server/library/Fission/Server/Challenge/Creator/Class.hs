module Fission.Challenge.Creator.Class (Creator (..)) where

import           Fission.Prelude
import           Fission.Error.Types
import qualified Fission.Random as Random

import           Fission.Models as Models
import           Database.Esqueleto

import           Fission.Challenge.Types


class Monad m => Creator m where
  create :: UserId -> m (Either (AlreadyExists UserChallenge) Challenge)

instance MonadIO m => Creator (Transaction m) where
  create userId = do
    challenge <- mkChallenge
    UserChallenge
      { userChallengeUserId = userId
      , userChallengeHash   = challenge
      }
      |> insertUnique
      |> bind  \case
        Nothing -> return $ Left AlreadyExists
        Just _  -> return $ Right challenge

mkChallenge :: MonadIO m => m Challenge
mkChallenge = Challenge <$> Random.alphaNum 32
