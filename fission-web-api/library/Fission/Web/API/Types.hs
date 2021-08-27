-- | Top-level router type for the application
module Fission.Web.API.Types (API, Unversioned, V2) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Types
import           Fission.Web.API.Auth.Types
import           Fission.Web.API.DNS.Types
import           Fission.Web.API.Heroku.Types
import           Fission.Web.API.IPFS.Types
import           Fission.Web.API.Ping.Types
import           Fission.Web.API.User.Types

type API = V2 :<|> Unversioned
type V2 = "api" :> "v2" :> Unversioned

-- DEPRECATED
type Unversioned = IPFS :<|> App :<|> Heroku :<|> User :<|> Ping :<|> DNS :<|> Auth
