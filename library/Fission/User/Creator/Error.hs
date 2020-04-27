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
  | ConflictingMultiple [AlreadyExists]
  deriving ( Show
           , Eq
           , Exception
           )

instance Display AlreadyExists where
  display = \case
    ConflictingUsername un ->
      "Username " <> un <> "already exists"
     
    ConflictingPublicKey pk ->
      "PublicKey " <> pk <> "already exists"

    ConflictingMultiple errs ->
      "Multiple conflicts: " <> fmap display errs

instance ToServerError AlreadyExists where
  toServerError err =
    err409 { errBody = displayLazyBS err }
