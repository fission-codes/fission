module Fission.Web.API.App.Types (App) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Create.Types
import           Fission.Web.API.App.Destroy.Types
import           Fission.Web.API.App.Index.Types
import           Fission.Web.API.App.Update.Streaming.Types
import           Fission.Web.API.App.Update.Types

type App = "app" :> API
type API = Index :<|> Create :<|> Update :<|> StreamingUpdate :<|> Destroy
