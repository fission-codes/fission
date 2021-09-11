module Fission.Web.API.User.WhoAmI.Types (Routes (..)) where

import           Fission.User.Username.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types  as Auth

newtype Routes mode = Routes
  { whoAmI ::
      mode
      :- Summary "Get username"
      :> Description "Get username registered to currently authenticated user"
      --
      :> Auth.HigherOrder
      :> Get '[PlainText, JSON] Username
  }
  deriving Generic
