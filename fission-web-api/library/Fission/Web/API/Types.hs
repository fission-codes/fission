-- | Top-level router type for the application
module Fission.Web.API.Types (API) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Types
import           Fission.Web.API.DNS.Types
import           Fission.Web.API.Heroku.Types
import           Fission.Web.API.IPFS.Types
import           Fission.Web.API.Ping.Types
import           Fission.Web.API.User.Types

type API = IPFS :<|> App :<|> Heroku :<|> User :<|> Ping :<|> DNS
