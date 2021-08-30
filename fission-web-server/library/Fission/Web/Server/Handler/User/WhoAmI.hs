module Fission.Web.Server.Handler.User.WhoAmI (handler) where

import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.WhoAmI.Types      as WhoAmI

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Models

handler :: Monad m => WhoAmI.Routes (AsServerT m)
handler  = WhoAmI.Routes { whoAmI }
  where
    whoAmI Authorization {about = Entity _ User { userUsername }} = return userUsername
