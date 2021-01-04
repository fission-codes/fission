module Fission.Web.API.User.DID.Types (DID) where

import qualified Fission.Key                as Key

import           Fission.Web.API.Prelude    hiding (Set)

import qualified Fission.Web.API.Auth.Types as Auth

type DID = "did" :> Set

type Set
  =  Auth.HigherOrder
  --
  :> Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  --
  :> ReqBody '[JSON] Key.Public
  :> PutNoContent
