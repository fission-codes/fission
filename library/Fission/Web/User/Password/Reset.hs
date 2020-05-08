module Fission.Web.User.Password.Reset
  ( API
  , server
  ) where

import           Database.Esqueleto
import           Servant

import           Fission.Prelude
import           Fission.Authorization

import qualified Fission.User          as User
import qualified Fission.User.Password as User.Password

import qualified Fission.Web.Error                     as Web.Err
import qualified Fission.Web.User.Password.Reset.Types as User.Password

type API
  =  Summary "Reset password"
  :> Description "DEPRECATED ⛔ Reset password"
  :> ReqBody '[JSON] User.Password.Reset
  :> Put     '[JSON] User.Password

server ::
  ( MonadThrow    m
  , MonadLogger   m
  , MonadTime     m
  , MonadIO       m
  , User.Modifier m
  )
  => Authorization
  -> ServerT API m
server Authorization {about = Entity userId _} User.Password.Reset { maybePassword } = do
  now      <- currentTime
  password <- maybe User.Password.random pure maybePassword
 
  User.updatePassword userId password now >>= \case
    Left  err         -> Web.Err.throw err
    Right updatedPass -> return updatedPass

  return password
