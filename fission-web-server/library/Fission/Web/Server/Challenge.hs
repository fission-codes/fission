module Fission.Web.Server.Challenge
  ( module Fission.Web.Server.Challenge.Types
  , module Fission.Web.Server.Challenge.Creator.Class
  , module Fission.Web.Server.Challenge.Verifier.Class
  , verificationLink
  ) where

import           Fission.Web.Server.Challenge.Creator.Class
import           Fission.Web.Server.Challenge.Types
import           Fission.Web.Server.Challenge.Verifier.Class

import           Fission.Prelude
import           Servant                                     hiding (route)

import qualified Fission.Internal.API                        as API
import qualified Fission.Web.Routes                          as Web
import qualified Fission.Web.User                            as User

verificationLink :: Challenge -> Text
verificationLink challenge =
  toUrlPiece $ API.mkLink route challenge
  where
    route = Proxy @(Web.UserPrefix :> User.VerifyEmailRoute)
