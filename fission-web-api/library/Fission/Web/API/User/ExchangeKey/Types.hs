module Fission.Web.API.User.ExchangeKey.Types
  ( Routes (..)
  , Add
  , Remove
  ) where

import qualified Crypto.PubKey.RSA          as RSA

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

data Routes mode = Routes
  { add ::
      mode :- Add

  , remove ::
      mode :- Remove
  }
  deriving Generic

type Add
  =  Summary "Add Public Exchange Key"
  :> Description "Add a key to the currently authenticated user's root list of public exchange keys"
  --
  :> Capture "did" RSA.PublicKey
  --
  :> Auth.HigherOrder
  :> Put '[JSON] (NonEmpty RSA.PublicKey)

type Remove
  =  Summary "Remove Public Exchange Key"
  :> Description "Remove a key from the currently authenticated user's root list of public exchange keys"
  --
  :> Capture "did" RSA.PublicKey
  --
  :> Auth.HigherOrder
  :> Delete  '[JSON] [RSA.PublicKey]
