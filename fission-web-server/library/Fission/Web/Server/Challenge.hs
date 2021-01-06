module Fission.Web.Server.Challenge
  ( module Fission.Web.Server.Challenge.Types
  , module Fission.Web.Server.Challenge.Creator.Class
  , module Fission.Web.Server.Challenge.Verifier.Class
  , verificationLink
  ) where

import           Servant                                     hiding (route)

import           Fission.Prelude

import           Fission.Challenge.Types

import           Fission.Web.Server.Challenge.Creator.Class
import           Fission.Web.Server.Challenge.Verifier.Class
import qualified Fission.Web.Server.User                     as User

verificationLink :: Challenge -> Text
verificationLink challenge =
  toUrlPiece $ API.mkLink route challenge
  where
    route = Proxy @(Web.UserPrefix :> User.VerifyEmailRoute)
