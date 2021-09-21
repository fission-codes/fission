module Fission.Web.Server.Fixture.User (user) where

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import           Fission.Prelude

import qualified Fission.Internal.Fixture.Key.Ed25519 as Ed25519
import           Fission.Internal.Fixture.Time
import qualified Fission.Internal.RSA2048.Pair.Types  as RSA2048

import           Fission.Web.Server.Models
import qualified Fission.Web.Server.User.Role.Types   as User.Roles

user :: User
user = User
  { userEmail    = Just "test@fission.codes"
  , userUsername = "testUser"
  --
  , userRole      = User.Roles.Regular
  , userActive    = True
  , userVerified  = False
  --
  , userIonID        = Nothing
  , userPublicKey    = Just Ed25519.pk
  , userExchangeKeys = Just [RSA2048.pk1]
  --
  , userDataRoot     = CID "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"
  , userDataRootSize = Bytes 0
  --
  , userHerokuAddOnId = Nothing
  , userSecretDigest  = Nothing
  --
  , userInsertedAt = agesAgo
  , userModifiedAt = agesAgo
  }

