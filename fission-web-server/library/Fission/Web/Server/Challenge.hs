module Fission.Web.Server.Challenge
  ( module Fission.Web.Server.Challenge.Creator.Class
  , module Fission.Web.Server.Challenge.Verifier.Class
  , module Fission.Web.Server.Challenge.Retriever.Class
  , verificationLink
  ) where

import           Servant                                      hiding (route)

import           Fission.Prelude

import           Fission.Challenge.Types

import qualified Fission.Web.API.User.Email.Verify.Types      as API.User.Email

import           Fission.Web.Server.Challenge.Creator.Class
import           Fission.Web.Server.Challenge.Verifier.Class
import           Fission.Web.Server.Challenge.Retriever.Class

import qualified Fission.Web.Server.Link                      as API

verificationLink :: Challenge -> Text
verificationLink challenge = toUrlPiece $ API.mkLink route challenge
  where
    route = Proxy @("user" :> "email" :> API.User.Email.Verify)
