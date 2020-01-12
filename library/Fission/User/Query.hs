module Fission.User.Query (Queryable (..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude

class Queryable m where
  getByUsername          :: Text          -> m (Maybe (Entity User))
  getHerkouAddonByUserId :: HerokuAddOnId -> m (Maybe (Entity User))
  getHerkouAddonByUUID   :: UUID          -> m (Maybe (Entity HerokuAddOn))

instance MonadIO m => Queryable (Transaction m) where
  getByUsername :: Text -> Transaction m (Maybe (Entity User))
  getByUsername username = selectFirst
    [ UserUsername P.==. username
    , UserActive   P.==. True
    ] []

  getHerkouAddonByUserId :: HerokuAddOnId -> Transaction m (Maybe (Entity User))
  getHerkouAddonByUserId addOnId = selectFirst
    [ UserHerokuAddOnId P.==. (Just addOnId)
    , UserActive P.==. True
    ] []

  getHerkouAddonByUUID :: UUID -> Transaction m (Maybe (Entity HerokuAddOn))
  getHerkouAddonByUUID uuid =
    selectFirst [HerokuAddOnUuid P.==. uuid] []
