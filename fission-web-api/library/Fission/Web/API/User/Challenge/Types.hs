module Fission.Web.API.User.Challenge.Types (Routes (..)) where

import           Fission.Challenge.Types

import           Fission.Web.API.Prelude

import           Fission.User.Username.Types

import qualified Fission.Web.API.Auth.Types  as Auth

data Routes mode = Routes
  { recover ::
      mode
      :- "recover"
      :> Summary "Return challenge for account recovery"
      --
      :> Capture "Username" Username
      --
      :> Auth.HigherOrder
      :> Post '[JSON] Challenge
  }
  deriving Generic
