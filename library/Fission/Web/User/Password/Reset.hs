module Fission.Web.User.Password.Reset
  ( API
  , server
  ) where

import           Database.Selda as Selda
import           Servant

import           Fission.Prelude
import           Fission.User                as User
import qualified Fission.User.Password.Types as User
import qualified Fission.User.Password       as User.Password

import           Fission.Web.Server
import qualified Fission.Web.Error                     as Web.Err
import qualified Fission.Web.User.Password.Reset.Types as User.Password

type API = ReqBody '[JSON] User.Password.Reset
        :> Put     '[JSON] User.Password

server :: (HasLogFunc cfg, MonadSelda (RIO cfg)) => User -> RIOServer cfg API
server User { userID } User.Password.Reset { maybePassword } = do
  password <- maybe User.Password.random pure maybePassword
  updated  <- User.updatePassword userID password

  case updated of
    Left err -> Web.Err.throw err
    Right updatedPass -> return updatedPass
