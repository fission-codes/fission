{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Password.Reset
  ( API
  , server
  ) where

import           RIO

import           Database.Selda as Selda

import           Servant

import           Fission.Web.Server
import qualified Fission.Web.Error    as Web.Err

import           Fission.User                 as User
import qualified Fission.User.Password.Types  as User

type API = ReqBody '[JSON] User.Password
        :> Put '[JSON] User.Password


server :: HasLogFunc         cfg
       => MonadSelda    (RIO cfg)
       => User
       -> RIOServer          cfg API
server User { _userID } password =
  User.updatePassword _userID password >>= \case
    Right updatedPass -> return updatedPass
    Left err -> Web.Err.throw err
