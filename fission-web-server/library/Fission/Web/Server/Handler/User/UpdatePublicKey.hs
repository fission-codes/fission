module Fission.Web.Server.Handler.User.UpdatePublicKey (handler) where

import           Servant

import           Fission.Prelude

import           Fission.Authorization

import qualified Fission.Key              as Key

import qualified Fission.Web.Server.Error as Web.Error
import qualified Fission.Web.Server.User  as User

handler ::
  ( MonadTime     m
  , MonadLogger   m
  , MonadThrow    m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userID _} pk = do
  now <- currentTime
  Web.Error.ensureM $ User.updatePublicKey userID pk now
  return NoContent
