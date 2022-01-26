module Fission.Web.Server.User.Creator.Error (AlreadyExists (..)) where

import           Servant.Server

import           Crypto.Key.Asymmetric.Public.Types as Key

import           Fission.Prelude

import           Fission.User.Username.Types

import           Fission.Web.Server.Error

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
