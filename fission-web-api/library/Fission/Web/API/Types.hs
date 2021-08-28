-- | Top-level router type for the application
module Fission.Web.API.Types (API, V2, V_) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Types
import           Fission.Web.API.Auth.Types
import           Fission.Web.API.DNS.Types
import           Fission.Web.API.Heroku.Types
import           Fission.Web.API.IPFS.Types
import           Fission.Web.API.Ping.Types
import           Fission.Web.API.User.Types

type API = V2 :<|> V_
type V2 = "v2" :> "api" :> V_

-- DEPRECATED
type V_ = IPFS :<|> App :<|> Heroku :<|> User :<|> Ping :<|> DNS :<|> Auth
