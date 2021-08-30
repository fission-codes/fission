module Fission.Web.Server.Handler.User.DataRoot (handler) where

import           Servant
import           Servant.Server.Generic

import           Fission.Prelude

import qualified Fission.Web.API.User.DataRoot.Types    as DataRoot

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error               as Web.Error
import qualified Fission.Web.Server.User                as User
import           Fission.Web.Server.WNFS                as WNFS

handler ::
  ( MonadLogger   m
  , MonadThrow    m
  , MonadTime     m
  , MonadWNFS     m
  , User.Modifier m
  )
  => DataRoot.Routes (AsServerT m)
handler = DataRoot.Routes {..}
  where
    get username =
      Web.Error.ensureM $ WNFS.getUserDataRoot username

    update newCID Authorization {about = Entity userID _} = do
      now <- currentTime
      Web.Error.ensureM $ User.setData userID newCID now
      return NoContent
