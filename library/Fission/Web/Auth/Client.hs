module Fission.Web.Auth.Client 
  ( verify
  , register
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.Web.Routes    as Web
import qualified Fission.Platform.User as User

verify :: BasicAuthData -> ClientM Bool
verify = client (Proxy :: Proxy Web.AuthRoute)

register :: ClientM User.Config
register = client (Proxy :: Proxy Web.RegisterRoute)
