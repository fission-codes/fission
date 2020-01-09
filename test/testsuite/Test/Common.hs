module Test.Common (agesAgo, testUser, testUserEntity) where

import           Data.Time as Time
import qualified Database.Persist as Database
import qualified Database.Persist.Sql as Database

-- ⚛️

import           Fission.Models
import qualified Fission.User.Role.Types as User.Roles
import           Fission.Prelude


-- TIME


agesAgo :: UTCTime
agesAgo = UTCTime
  { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
  , utctDayTime = Time.secondsToDiffTime 0
  }



-- USERS


testUser :: User
testUser = User
  { userUsername = "testUser"
  , userEmail = Just "test@fission.codes"
  , userRole = User.Roles.Regular
  , userActive = True

  --
  , userHerokuAddOnId = Nothing
  , userSecretDigest = "SECRET_DIGEST"

  --
  , userInsertedAt = agesAgo
  , userModifiedAt = agesAgo
  }

testUserEntity :: Database.Entity User
testUserEntity = Database.Entity
  { entityKey = Database.toSqlKey 0
  , entityVal = testUser
  }
