module Fission.Web.Auth.Client (verify) where

import RIO

import qualified Network.HTTP.Client as HTTP

import Servant
import Servant.Client

import qualified Fission.Web.Routes as Web

verify :: BasicAuthData -> ClientM Bool
verify = client (Proxy :: Proxy Web.AuthRoute)
