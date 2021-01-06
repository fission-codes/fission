module Fission.Web.Server.Handler.User.DataRoot.Update (handler) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.DataRoot.Update.Types as API.DataRoot

import           Fission.Web.Server.Authorization.Types
import           Fission.Web.Server.Error                   as Web.Error
import qualified Fission.Web.Server.User                    as User

handler :: (MonadLogger m, MonadThrow m, MonadTime m, User.Modifier m) => ServerT API.DataRoot.UpdateRoot m
handler newCID Authorization {about = Entity userID _} = do
  now <- currentTime
  Web.Error.ensureM $ User.setData userID newCID now
  return NoContent
