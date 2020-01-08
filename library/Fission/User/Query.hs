module Fission.User.Query
  ( getHerkouAddonByUUID
  , getHerkouAddonByUserId
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude
import           Fission.Storage.Query.Class

-- getHerkouAddonByUserId :: MonadDB m => HerokuAddOnId -> Transaction m (Maybe (Entity User))
-- getHerkouAddonByUserId addOnId = selectFirst [UserHerokuAddOnId P.==. (Just addOnId), UserActive P.==. True] []

-- getHerkouAddonByUUID :: MonadDB m => UUID -> Transaction m (Maybe (Entity HerokuAddOn))
-- getHerkouAddonByUUID uuid = selectFirst [HerokuAddOnUuid P.==. uuid] []
getHerkouAddonByUserId :: MonadDBQuery User m => HerokuAddOnId -> Transaction m [Entity User]
getHerkouAddonByUserId addOnId =
      getOneBy (\user ->
                  user ^. UserHerokuAddOnId ==. val (Just addOnId)
                  &&. user ^. UserActive        ==. val True)

getHerkouAddonByUUID :: MonadDBQuery HerokuAddOn m => UUID -> Transaction m [Entity HerokuAddOn]
getHerkouAddonByUUID uuid =
      getOneBy (\herokuAddOn -> herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
