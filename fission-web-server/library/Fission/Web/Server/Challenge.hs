module Fission.Web.Server.Challenge
  ( module Fission.Web.Server.Challenge.Creator.Class
  , module Fission.Web.Server.Challenge.Verifier.Class
  , module Fission.Web.Server.Challenge.Retriever.Class
  , verificationLink
  ) where

import           Servant

import           Fission.Prelude

import           Fission.Challenge.Types

import qualified Fission.Web.API.User.Email.Verify.Types      as API.User.Email

import           Fission.Web.Server.Challenge.Creator.Class
import           Fission.Web.Server.Challenge.Retriever.Class
import           Fission.Web.Server.Challenge.Verifier.Class

import qualified Fission.Web.Server.Link                      as API

type Route
  = "v2"
  :> "api"
  :> "user"
  :> "email"
  :> API.User.Email.Verify

verificationLink :: Challenge -> Text
verificationLink challenge = toUrlPiece $ API.mkLink (Proxy @Route) challenge
