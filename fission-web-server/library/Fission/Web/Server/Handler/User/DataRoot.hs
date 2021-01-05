module Fission.Web.Server.Handler.User.DataRoot (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.User                            as User

import           Fission.Web.Server.WNFS

import qualified Fission.Web.Server.Auth.Types           as Auth
import qualified Fission.Web.Server.User.DataRoot.Get    as Get
import qualified Fission.Web.Server.User.DataRoot.Update as Update

handler ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => ServerT API m
handler = Update.server :<|> Get.server
