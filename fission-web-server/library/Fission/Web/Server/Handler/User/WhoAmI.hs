module Fission.Web.Server.Handler.User.WhoAmI (handler) where

import           Servant

import           Fission.Prelude

import           Fission.Authorization
import           Fission.User.Username.Types

import           Fission.Web.Server.Models

handler :: Monad m => Authorization -> ServerT API m
handler Authorization {about = Entity _ User { userUsername }} = return userUsername
