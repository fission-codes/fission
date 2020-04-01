module Fission.Internal.Fixture.User (user) where

import           Network.IPFS.CID.Types

import           Fission.Prelude
import qualified Fission.Key as Key
import           Fission.Models

import qualified Fission.User.Role.Types as User.Roles

import           Fission.Internal.Fixture.Time

user :: User
user = User
  { userUsername = "testUser"
  , userEmail    = Just "test@fission.codes"

  --

  , userRole   = User.Roles.Regular
  , userActive = True

  --

  , userPublicKey = Just $ Key.Public "1498b5467a63dffa2dc9d9e069caf075d16fc33fdd4c3b01bfadae6433767d93"
  , userAlgorithm = Just Key.Ed25519
  , userDataRoot  = CID "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"

  --

  , userHerokuAddOnId = Nothing
  , userSecretDigest  = Nothing

  --

  , userInsertedAt = agesAgo
  , userModifiedAt = agesAgo
  }
