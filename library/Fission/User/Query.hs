module Fission.User.Query (MonadDBQuery(..)) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude
import           Fission.Types

class MonadDB m => MonadDBQuery m where
  getByUsername          :: Text -> Transaction m (Maybe (Entity User))
  getHerkouAddonByUserId :: HerokuAddOnId -> Transaction m (Maybe (Entity User))
  getHerkouAddonByUUID   :: UUID -> Transaction m (Maybe (Entity HerokuAddOn))

instance MonadDBQuery Fission where
  getByUsername username = selectFirst
    [ UserUsername P.==. username
    , UserActive   P.==. True
    ] []

  getHerkouAddonByUserId addOnId = selectFirst
    [ UserHerokuAddOnId P.==. (Just addOnId)
    , UserActive P.==. True
    ] []

  getHerkouAddonByUUID uuid =
    selectFirst [HerokuAddOnUuid P.==. uuid] []
