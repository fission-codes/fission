module Fission.Web.API.User.ExchangeKey.Types
  ( ExchangeKeys
  , Add
  , Remove
  ) where

import qualified Crypto.PubKey.RSA          as RSA

import           Fission.Web.API.Prelude

import qualified Fission.Web.API.Auth.Types as Auth

type ExchangeKeys = "exchange" :> "keys" :> API

type API = Add :<|> Remove

type Add
  =  Auth.HigherOrder
  --
  :> Summary "Add Public Exchange Key"
  :> Description "Add a key to the currently authenticated user's root list of public exchange keys"
  --
  :> Capture "did" RSA.PublicKey
  :> Put     '[JSON] (NonEmpty RSA.PublicKey)

type Remove
  =  Auth.HigherOrder
  --
  :> Summary "Remove Public Exchange Key"
  :> Description "Remove a key from the currently authenticated user's root list of public exchange keys"
  --
  :> Capture "did" RSA.PublicKey
  :> Delete  '[JSON] [RSA.PublicKey]
