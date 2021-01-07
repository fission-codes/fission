module Fission.Web.Server.Handler.User.DataRoot (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.DataRoot.Types             as API

import qualified Fission.Web.Server.User                         as User
import           Fission.Web.Server.WNFS

import qualified Fission.Web.Server.Handler.User.DataRoot.Get    as Get
import qualified Fission.Web.Server.Handler.User.DataRoot.Update as Update

handler ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => ServerT API.DataRoot m
handler = Update.handler :<|> Get.handler
