module Fission.Web.Server.Challenge.Creator.Class (Creator (..)) where

import           Database.Esqueleto

import           Fission.Prelude

import qualified Fission.Random                     as Random

import           Fission.Web.Server.Challenge.Types
import           Fission.Web.Server.Error.Types
import           Fission.Web.Server.Models          as Models

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
