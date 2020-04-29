module Fission.User.Creator.Error (AlreadyExists (..)) where

import           Servant.Server

import           Fission.Prelude
import           Fission.Web.Error

import           Fission.User.Username.Types
import           Fission.Key.Asymmetric.Public.Types as Key

data AlreadyExists
  = ConflictingUsername  Username
  | ConflictingPublicKey Key.Public
  -- TODO | ConflictingEmail Email
  deriving ( Show
           , Eq
           , Exception
           )

instance Display AlreadyExists where
  display = \case
    ConflictingUsername un ->
      "Username " <> display un <> " already exists"
     
    ConflictingPublicKey pk ->
      "Public key " <> display pk <> " already exists"

instance ToServerError AlreadyExists where
  toServerError err =
    err409 { errBody = displayLazyBS err }
