module Fission.Internal.Fixture.User (user) where

import           Fission.Prelude
import           Fission.Models

import qualified Fission.User.Role.Types as User.Roles
import           Fission.User.DID.Types

import           Fission.Internal.Fixture.Time

user :: User
user = User
  { userUsername = "testUser"
  , userEmail    = Just "test@fission.codes"
  , userRole     = User.Roles.Regular
  , userActive   = True
  , userDid      = Just <| DID "1498b5467a63dffa2dc9d9e069caf075d16fc33fdd4c3b01bfadae6433767d93"

  --
  , userHerokuAddOnId = Nothing
  , userSecretDigest  = Nothing

  --
  , userInsertedAt = agesAgo
  , userModifiedAt = agesAgo
  }
