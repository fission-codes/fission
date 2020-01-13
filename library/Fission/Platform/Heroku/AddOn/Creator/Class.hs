module Fission.Platform.Heroku.AddOn.Creator.Class (Creator (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude

import           Fission.Platform.Heroku.Region.Types
import           Fission.Platform.Heroku.AddOn.Creator.Error

class Monad m => Creator m where
  create :: UUID -> Region -> UTCTime -> m (Either Error HerokuAddOnId)

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
