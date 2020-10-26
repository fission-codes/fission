module Fission.Web.User.WhoAmI
  ( API
  , server
  ) where

import           Servant

import           Fission.Prelude

import qualified Fission.Authorization                      as Authorization
import           Fission.Web.Auth.Token.UCAN.Resource.Types

import           Fission.Models
import           Fission.User.Username.Types

type API
  =  Summary "Get username"
  :> Description "Get username registered to currently authenticated user"
  :> Get '[PlainText, JSON] Username

server :: Monad m => Authorization.Session -> ServerT API m
server Authorization.Session {} = do
-- server Authorization {about = Entity _ User { userUsername }} =
  let userUsername = undefined -- FIXME
  return userUsername
