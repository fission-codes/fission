module Fission.Web.API.App.Index.Types
  ( Index
  , module Fission.Web.API.App.Index.Payload.Types
  ) where

import           Fission.Web.API.Prelude

import           Fission.Web.API.App.Index.Payload.Types
import qualified Fission.Web.API.Auth.Types              as Auth

type Index
  =  Summary "App index"
  :> Description "A list of all of your apps and their associated domain names"
  --
  :> Auth.HigherOrder
  :> Get '[JSON] (Map Natural Payload)
