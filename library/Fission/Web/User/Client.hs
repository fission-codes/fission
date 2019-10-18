module Fission.Web.User.Client
  ( register
  , verify
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.User.Provision.Types as User
import           Fission.Web.Routes           (UserRoute)
import qualified Fission.Web.User             as User

import           Fission.Internal.Orphanage.BasicAuthData ()

verify   :: BasicAuthData -> ClientM Bool
register :: BasicAuthData -> ClientM User.Provision

register :<|> verify = client (Proxy :: Proxy UserRoute)
