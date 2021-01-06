module Fission.Web.Server.Handler.User.WhoAmI (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.WhoAmI.Types      as API

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Models

handler :: Monad m => ServerT API.WhoAmI m
handler Authorization {about = Entity _ User { userUsername }} = return userUsername
