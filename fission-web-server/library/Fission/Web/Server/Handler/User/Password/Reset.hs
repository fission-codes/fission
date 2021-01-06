module Fission.Web.Server.Handler.User.Password.Reset (handler) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import qualified Fission.User.Password.Reset.Types         as User.Password

import qualified Fission.Web.API.User.Password.Reset.Types as API.Password

import           Fission.Web.Server.Authorization.Types
import qualified Fission.Web.Server.Error                  as Web.Err
import qualified Fission.Web.Server.User                   as User
import qualified Fission.Web.Server.User.Password          as User.Password

handler :: (MonadThrow m, MonadLogger m, MonadTime m, MonadIO m, User.Modifier m) => ServerT API.Password.Reset m
handler User.Password.Reset { maybePassword } Authorization {about = Entity userId _} = do
  now      <- currentTime
  password <- maybe User.Password.random pure maybePassword

  User.updatePassword userId password now >>= \case
    Left  err         -> Web.Err.throw err
    Right updatedPass -> return updatedPass

  return password
