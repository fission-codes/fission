module Fission.Web.User.Client
  ( register
  , verify
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.Web.User as User
import qualified Fission.User.Provision.Types as User
import Fission.Web.Routes (UserPrefix)

type API = UserPrefix :> User.API
  
verify :: BasicAuthData -> ClientM Bool
register :: ClientM User.Provision

register :<|> verify = client (Proxy :: Proxy API)
