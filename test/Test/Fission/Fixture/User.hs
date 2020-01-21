module Test.Fission.Fixture.User
  ( testUser
  ) where

import           Fission.Models
import qualified Fission.User.Role.Types as User.Roles
import           Fission.Prelude
import           Test.Fission.Fixture.Time

testUser :: User
testUser = User
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
