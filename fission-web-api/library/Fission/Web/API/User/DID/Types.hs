module Fission.Web.API.User.DID.Types (SetAuthenticatedV1, SetAuthenticatedV3, SetViaChallengeV1, SetViaChallengeV3, RoutesV3 (..), RoutesV_ (..)) where

import qualified Crypto.Key.Asymmetric.Public.Oldstyle.Types as Key.Public

import           Fission.Challenge.Types
import qualified Fission.Key                as Key
import           Fission.User.Username

import           Fission.Web.API.Prelude    hiding (Set)

import qualified Fission.Web.API.Auth.Types as Auth

-- V3

data RoutesV3 mode = RoutesV3
  { setAuthenticated :: mode :- SetAuthenticatedV3
  , setViaChallenge  :: mode :- SetViaChallengeV3
  }
  deriving Generic

type SetAuthenticatedV3
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  --
  :> ReqBody '[JSON] Key.Public
  --
  :> Auth.HigherOrder
  :> PutNoContent

type SetViaChallengeV3
  =  Summary "Update Public Key via Email Challenge"
  :> Description "Set given username's public key to something else via providing a challenge obtained from /user/email/recover"
  --
  :> ReqBody '[JSON] Key.Public
  :> Capture "Username" Username
  :> Capture "Challenge" Challenge
  --
  :> PutNoContent

-- V1

data RoutesV_ mode = RoutesV_
  { setAuthenticated :: mode :- SetAuthenticatedV1
  , setViaChallenge  :: mode :- SetViaChallengeV1
  }
  deriving Generic

type SetAuthenticatedV1
  =  Summary "Update Public Key"
  :> Description "Set currently authenticated user's root public key to another one"
  --
  :> ReqBody '[JSON] Key.Public.Oldstyle
  --
  :> Auth.HigherOrder
  :> PutNoContent

type SetViaChallengeV1
  =  Summary "Update Public Key via Email Challenge"
  :> Description "Set given username's public key to something else via providing a challenge obtained from /user/email/recover"
  --
  :> ReqBody '[JSON] Key.Public.Oldstyle
  :> Capture "Username" Username
  :> Capture "Challenge" Challenge
  --
  :> PutNoContent
