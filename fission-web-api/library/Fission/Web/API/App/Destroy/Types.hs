module Fission.Web.API.App.Destroy.Types
  ( Destroy
  , ByURL
  , ById
  ) where

import           Fission.URL.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type Destroy = ByURL :<|> ById

type ByURL
  =  Summary "Destroy app by URL"
  :> Description "Destroy app by any associated URL"
  --
  :> "associated"
  :> Capture "url" URL
  --
  :> Auth.HigherOrder
  :> DeleteNoContent

type ById
  =  Summary "Destroy app by ID"
  :> Description "Destroy app by its ID"
  --
  :> Capture "appId" Natural
  --
  :> Auth.HigherOrder
  :> DeleteNoContent
