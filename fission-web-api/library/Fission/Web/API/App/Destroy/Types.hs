module Fission.Web.API.App.Destroy.Types (Routes (..)) where

import           Fission.URL.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

data Routes mode = Routes
  { byURL ::
      mode
      :- Summary "Destroy app by URL"
      :> Description "Destroy app by any associated URL"
      --
      :> "associated"
      :> Capture "url" URL
      --
      :> Auth.HigherOrder
      :> DeleteNoContent

  , byID ::
      mode
      :- Summary "Destroy app by ID"
      :> Description "Destroy app by its ID"
      --
      :> Capture "appId" Natural
      --
      :> Auth.HigherOrder
      :> DeleteNoContent
  }
  deriving Generic
