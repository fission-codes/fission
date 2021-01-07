module Fission.Web.API.User.DataRoot.Types (DataRoot) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.User.DataRoot.Get.Types
import           Fission.Web.API.User.DataRoot.Update.Types

type DataRoot = "data" :> API

type API = UpdateRoot :<|> GetRoot
