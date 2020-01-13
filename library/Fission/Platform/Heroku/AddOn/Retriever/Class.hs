module Fission.Platform.Heroku.AddOn.Retriever.Class (Retriever (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude

class Monad m => Retriever m where
  getByUUID :: UUID -> m (Maybe (Entity HerokuAddOn))

instance MonadIO m => Retriever (Transaction m) where
  getByUUID uuid = selectFirst [HerokuAddOnUuid P.==. uuid] []
