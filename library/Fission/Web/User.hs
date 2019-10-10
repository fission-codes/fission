module Fission.Web.User
  ( server
  , API
  , VerifyRoute
  ) where

import RIO

import Data.Has
import Database.Selda

import Servant

import           Fission.Web.Server
import qualified Fission.Web.User.Create as Create
import qualified Fission.Web.User.Verify as Verify
import qualified Fission.Web.Auth        as Auth
import qualified Fission.Web.Types       as Web

type API = Create.API
          :<|> VerifyRoute

type VerifyRoute = "verify" 
                  :> Auth.ExistingUser
                  :> Verify.API

server :: HasLogFunc        cfg
       => MonadSelda   (RIO cfg)
        => Has Web.Host    cfg
       => RIOServer         cfg API
server = Create.server
    :<|> const Verify.server

