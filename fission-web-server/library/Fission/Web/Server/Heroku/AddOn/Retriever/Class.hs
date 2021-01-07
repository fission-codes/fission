module Fission.Web.Server.Heroku.AddOn.Retriever.Class (Retriever (..)) where

import           Data.UUID                        (UUID)
import           Database.Esqueleto               hiding (update, (=.))
import qualified Database.Persist                 as P

import           Fission.Prelude

import           Fission.Web.Server.Models
import           Fission.Web.Server.MonadDB.Types

class Monad m => Retriever m where
  getByUUID :: UUID -> m (Maybe (Entity HerokuAddOn))

instance MonadIO m => Retriever (Transaction m) where
  getByUUID uuid = selectFirst [HerokuAddOnUuid P.==. uuid] []
