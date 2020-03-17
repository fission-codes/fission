module Fission.Platform.Heroku.AddOn.Creator.Class (Creator (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Fission.Prelude
import           Fission.Error.Types
import           Fission.Models

import           Fission.Platform.Heroku.Region.Types

class Monad m => Creator m where
  create :: UUID -> Region -> UTCTime -> m (Either (AlreadyExists HerokuAddOn) HerokuAddOnId)

instance MonadIO m => Creator (Transaction m) where
  create uuid region now =
    HerokuAddOn
      { herokuAddOnUuid       = uuid
      , herokuAddOnRegion     = Just region
      , herokuAddOnInsertedAt = now
      , herokuAddOnModifiedAt = now
      }
      |> insertUnique
      |> bind  \case
        Nothing -> return (Left AlreadyExists)
        Just x  -> return (Right x)
