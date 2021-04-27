module Fission.Web.API.App.Index.Types (Index) where

import           Fission.App.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Index
  =  Summary "App index"
  :> Description "A list of all of your apps and their associated domain names"
  --
  :> Auth.HigherOrder
  :> Get '[JSON] (Map Natural Payload)
