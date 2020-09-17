module Fission.Web.User.DataRoot
  ( API
  , server
  ) where

import           Fission.Prelude
import           Servant

import qualified Fission.User as User

import           Fission.WNFS

import qualified Fission.Web.Auth.Types as Auth
import qualified Fission.Web.User.DataRoot.Update as Update
import qualified Fission.Web.User.DataRoot.Get as Get

type API = UpdateAPI :<|> Get.API

type UpdateAPI = 
  Auth.HigherOrder
  :> Update.API

server ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => ServerT API m
server = Update.server :<|> Get.server
