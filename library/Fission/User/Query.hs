module Fission.User.Query
  ( getHerkouAddonByUUID
  , getHerkouAddonByUserId
  ) where

import           Data.UUID (UUID)
import           Database.Esqueleto hiding ((=.), update)

import           Fission.Models
import           Fission.Prelude

getHerkouAddonByUserId :: MonadDB m => HerokuAddOnId -> Transaction m [Entity User]
getHerkouAddonByUserId addOnId = select <| from \user -> do
      where_ <| user ^. UserHerokuAddOnId ==. val (Just addOnId)
            &&. user ^. UserActive        ==. val True
      limit 1
      return user

getHerkouAddonByUUID :: MonadDB m => UUID -> Transaction m [Entity HerokuAddOn]
getHerkouAddonByUUID uuid = select <| from \herokuAddOn -> do
      where_ (herokuAddOn ^. HerokuAddOnUuid ==. val uuid)
      limit 1
      return herokuAddOn