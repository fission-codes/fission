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
import qualified Fission.Web.Types as Web

import qualified Fission.User                     as User
import qualified Fission.User.Provision.Types     as User
import qualified Fission.User.Registration.Types  as User

import           Fission.Security.Types (Secret (..))
  
type API = ReqBody '[JSON] User.Registration 
        :> Post '[JSON] User.Provision


server :: HasLogFunc      cfg
       => Has Web.Host    cfg
       => MonadSelda (RIO cfg)
       => RIOServer       cfg API
server (User.Registration username password email) = do
  Web.Host url <- Config.get
  userID       <- User.create username password email
  logInfo $ "Provisioned user: " <> displayShow userID

  return User.Provision
    { _url      = url
    , _username = username
    , _password = Secret password
    }
