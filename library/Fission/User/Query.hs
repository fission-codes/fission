module Fission.User.Query
  ( getHerkouAddonByUUID
  , getHerkouAddonByUserId
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)
import qualified Database.Persist as P

import           Fission.Models
import           Fission.Prelude
import           Fission.Storage

getHerkouAddonByUserId :: MonadDBQuery User m => HerokuAddOnId -> Transaction m (Maybe (Entity User))
getHerkouAddonByUserId addOnId =
  getOneBy [ UserHerokuAddOnId P.==. (Just addOnId)
           , UserActive P.==. True
           ]

-- TODO think about moving this to query
getHerkouAddonByUUID :: MonadDBQuery HerokuAddOn m => UUID -> Transaction m (Maybe (Entity HerokuAddOn))
getHerkouAddonByUUID uuid = getOneBy [HerokuAddOnUuid P.==. uuid]

-- getHerkouAddonByUserId :: MonadDBQuery User m => HerokuAddOnId -> Transaction m [Entity User]
-- getHerkouAddonByUserId addOnId =
--       getOneBy (\user ->
--                   user ^. UserHerokuAddOnId ==. val (Just addOnId)
--                   &&. user ^. UserActive        ==. val True)

-- getHerkouAddonByUUID :: MonadDBQuery HerokuAddOn m => UUID -> Transaction m [Entity HerokuAddOn]
-- getHerkouAddonByUUID uuid =
--       getOneBy (\herokuAddOn -> herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
