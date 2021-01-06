module Fission.Web.Server.Handler.User.UpdatePublicKey (handler) where

import           Servant

import           Fission.Prelude

import qualified Fission.Web.API.User.DID.Types         as API

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error               as Web.Error
import qualified Fission.Web.Server.User                as User

handler :: (MonadTime m, MonadLogger m, MonadThrow m, User.Modifier m) => ServerT API.DID m
handler pk Authorization {about = Entity userID _} = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent
