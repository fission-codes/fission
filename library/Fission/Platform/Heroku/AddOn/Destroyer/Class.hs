module Fission.Platform.Heroku.AddOn.Destroyer.Class (Destroyer (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto

import           Fission.Models
import           Fission.Prelude

-- | Destroy @Heroku.AddOn@s
class Monad m => Destroyer m where
  destroyByUUID :: UUID -> m ()

instance MonadIO m => Destroyer (Transaction m) where
  destroyByUUID uuid = delete <| from \herokuAddOn ->
    where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
