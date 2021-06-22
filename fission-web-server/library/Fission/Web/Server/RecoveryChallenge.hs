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


recoveryLink :: Text -> Challenge -> Text
recoveryLink recoveryAppUrl challenge =
  recoveryAppUrl <> "?challenge=" <> toUrlPiece challenge
