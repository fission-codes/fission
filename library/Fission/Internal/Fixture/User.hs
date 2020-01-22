module Fission.Internal.Fixture.User (user) where

import           Fission.Models
import qualified Fission.User.Role.Types as User.Roles
import           Fission.Prelude
import           Fission.Internal.Fixture.Time

user :: User
user = User
  { userUsername = "testUser"
  , userEmail    = Just "test@fission.codes"
  , userRole     = User.Roles.Regular
  , userActive   = True

  --
  , userHerokuAddOnId = Nothing
  , userSecretDigest  = "SECRET_DIGEST"

  --
  , userInsertedAt = agesAgo
  , userModifiedAt = agesAgo
  }
