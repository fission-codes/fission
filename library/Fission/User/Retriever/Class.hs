module Fission.User.Retriever.Class (Retriever (..)) where

import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude

class Monad m => Retriever m where
  getByUsername      :: Text          -> m (Maybe (Entity User))
  getByHerokuAddOnId :: HerokuAddOnId -> m (Maybe (Entity User))

instance MonadIO m => Retriever (Transaction m) where
  getByUsername username = selectFirst
    [ UserUsername P.==. username
    , UserActive   P.==. True
    ] []

  getByHerokuAddOnId addOnId = selectFirst
    [ UserHerokuAddOnId P.==. Just addOnId
    , UserActive        P.==. True
    ] []
