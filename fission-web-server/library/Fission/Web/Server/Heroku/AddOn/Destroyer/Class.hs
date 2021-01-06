module Fission.Web.Server.Heroku.AddOn.Destroyer.Class (Destroyer (..)) where

import           Data.UUID                        (UUID)
import           Database.Esqueleto

import           Fission.Prelude

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

-- | Destroy @Heroku.AddOn@s
class Monad m => Destroyer m where
  destroyByUUID :: UUID -> m ()

instance MonadIO m => Destroyer (Transaction m) where
  destroyByUUID uuid = delete $ from \herokuAddOn ->
    where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
