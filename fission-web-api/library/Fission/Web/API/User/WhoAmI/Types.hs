module Fission.Web.API.User.WhoAmI.Types (WhoAmI) where

import           Fission.User.Username.Types

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types  as Auth

type WhoAmI = "whoami" :> Check

type Check
  =  Auth.HigherOrder
  --
  :> Summary "Get username"
  :> Description "Get username registered to currently authenticated user"
  --
  :> Get '[PlainText, JSON] Username
