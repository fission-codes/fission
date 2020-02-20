{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

module Fission.Models where

import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Data.UUID
import           Data.Swagger

import           Network.IPFS.CID.Types

import           Fission.Prelude

import           Fission.Platform.Heroku.Region.Types
import           Fission.Security

import           Fission.User.DID.Types
import           Fission.User.Role.Types
import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Internal.Orphanage.CID  ()
import           Fission.Internal.Orphanage.UUID ()

import qualified Fission.Internal.UTF8 as UTF8

-- NB: The `!force` on user email uniqueness is due to a warning that
--     NULLs are not consider equal. This is what we want.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
HerokuAddOn
  uuid       UUID         sqltype=uuid
  region     Region Maybe

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueUUID uuid
  deriving Show Eq

User
  didAlgorithm  DID.Algorithm Maybe
  didPK         DID.PublicKey Maybe

  username      Username
  email         Email         Maybe
  role          Role
  active        Bool
  herokuAddOnId HerokuAddOnId Maybe
  secretDigest  SecretDigest  Maybe

  insertedAt    UTCTime
  modifiedAt    UTCTime

  UniqueUsername username
  UniqueEmail    email !force

  deriving Show Eq

UserCid
  userFk     UserId
  cid        CID

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueUserCid userFk cid
  deriving Show Eq
|]

instance Arbitrary UserId where
  arbitrary = toSqlKey <$> arbitrary

instance Digestable UserId where
  digest = digest . UTF8.textShow

instance ToSchema UserId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserID")
      |> pure
