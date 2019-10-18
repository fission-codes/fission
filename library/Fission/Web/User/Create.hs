{-# LANGUAGE MonoLocalBinds    #-}

module Fission.Web.User.Create
  ( API
  , server
  ) where

import           RIO

import           Database.Selda as Selda

import           Servant
import           Data.Has
import qualified Fission.Config as Config

import           Fission.Web.Server
import qualified Fission.Web.Types       as Web

import qualified Fission.Random as Random

import qualified Fission.User                 as User
import qualified Fission.User.Provision.Types as User

import           Fission.Security.Types (Secret (..))

type API = ReqBody '[JSON] BasicAuthData
        :> Post '[JSON] User.Provision

server :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
server (BasicAuthData username password) = do
  userID       <- User.create username secret
  logInfo $ "Provisioned user: " <> displayShow userID

  return User.Provision
    { _url      = url
    , _username = username
    , _password = Secret password
    }
