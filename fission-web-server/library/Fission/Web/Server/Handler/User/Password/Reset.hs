module Fission.Web.Server.Handler.User.Password.Reset (handler) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude

import           Fission.Authorization

import qualified Fission.Web.API.User.Password                as User.Password

import qualified Fission.Web.Server.Error                     as Web.Err
import qualified Fission.Web.Server.User                      as User
import qualified Fission.Web.Server.User.Password.Reset.Types as User.Password

handler ::
  ( MonadThrow    m
  , MonadLogger   m
  , MonadTime     m
  , MonadIO       m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
handler Authorization {about = Entity userId _} User.Password.Reset { maybePassword } = do
  now      <- currentTime
  password <- maybe User.Password.random pure maybePassword

  User.updatePassword userId password now >>= \case
    Left  err         -> Web.Err.throw err
    Right updatedPass -> return updatedPass

  return password
