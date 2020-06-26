module Fission.Challenge
  ( module Fission.Challenge.Types
  , module Fission.Challenge.Creator.Class
  , module Fission.Challenge.Verifier.Class
  , verificationLink
  ) where

import Fission.Challenge.Types
import Fission.Challenge.Creator.Class
import Fission.Challenge.Verifier.Class

import Fission.Prelude
import Servant         hiding (route)

import qualified Fission.Web.Routes   as Web 
import qualified Fission.Internal.API as API
import qualified Fission.Web.User     as User

verificationLink :: Challenge -> Text
verificationLink challenge = 
  toUrlPiece $ API.mkLink route challenge
  where 
    route = Proxy @(Web.UserPrefix :> User.VerifyEmailRoute)
