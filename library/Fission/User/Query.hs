module Fission.User.Query
  ( getHerkouAddonByUUID
  , getHerkouAddonByUserId
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude

getHerkouAddonByUserId :: MonadDB m => HerokuAddOnId -> Transaction m (Maybe (Entity User))
getHerkouAddonByUserId addOnId = selectFirst [UserHerokuAddOnId P.==. (Just addOnId), UserActive P.==. True] []

getHerkouAddonByUUID :: MonadDB m => UUID -> Transaction m (Maybe (Entity HerokuAddOn))
getHerkouAddonByUUID uuid = selectFirst [HerokuAddOnUuid P.==. uuid] []
