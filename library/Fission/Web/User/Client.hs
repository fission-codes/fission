module Fission.Web.User.Client
  ( register
  , verify
  ) where

import RIO

import Servant
import Servant.Client

import qualified Fission.User.Registration.Types as User
import qualified Fission.User.Provision.Types    as User
import           Fission.Web.Routes              (UserRoute)

import           Fission.Internal.Orphanage.BasicAuthData ()

verify   :: BasicAuthData     -> ClientM Bool
register :: User.Registration -> ClientM User.Provision

register :<|> verify = client $ Proxy @UserRoute
