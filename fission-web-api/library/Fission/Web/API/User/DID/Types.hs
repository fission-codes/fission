module Fission.Web.API.User.DID.Types (DID, SetAuthenticated, SetViaChallenge) where

import           Fission.Challenge.Types
import qualified Fission.Key                as Key
import           Fission.User.Username

import           Fission.Web.API.Prelude    hiding (Set)

import qualified Fission.Web.API.Auth.Types as Auth

type DID = "did" :> (SetAuthenticated :<|> SetViaChallenge)

type SetAuthenticated
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  --
  :> ReqBody '[JSON] Key.Public
  --
  :> Auth.HigherOrder
  :> PutNoContent

type SetViaChallenge
  =  Summary "Update Public Key via Email Challenge"
  :> Description "Set given username's public key to something else via providing a challenge obtained from /user/email/recover"
  --
  :> ReqBody '[JSON] Key.Public
  :> Capture "Username" Username
  :> QueryParams "Challenge" Challenge
  --
  :> PutNoContent
