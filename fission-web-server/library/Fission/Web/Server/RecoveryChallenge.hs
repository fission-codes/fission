module Fission.Web.Server.RecoveryChallenge
  ( module Fission.Web.Server.RecoveryChallenge.Creator.Class
  , module Fission.Web.Server.RecoveryChallenge.Retriever.Class
  , recoveryLink
  ) where

import           Servant                                              hiding
                                                                      (route)

import           Fission.Prelude

import           Fission.Challenge.Types

import           Fission.Web.Server.RecoveryChallenge.Creator.Class
import           Fission.Web.Server.RecoveryChallenge.Retriever.Class


type RecoveryAPI =
    "recover" :> QueryParam' '[Required, Strict] "challenge" Challenge :> Get '[PlainText] Text


recoveryLink :: Challenge -> Text
recoveryLink challenge = toUrlPiece $ safeLink (Proxy @RecoveryAPI) route challenge
  where
    route = Proxy @("recover" :> QueryParam' '[Required, Strict] "challenge" Challenge :> Get '[PlainText] Text)
