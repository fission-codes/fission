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
  did           DID           Maybe
  email         Email         Maybe
  username      Username

  role          Role
  active        Bool

  dataRoot      CID

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

App
  owner            UserId

  name             AppName
  description      Description

  cid              CID
  fissionSubdomain Subdomain

  insertedAt       UTCTime
  modifiedAt       UTCTime

  deriving Show Eq

AppDomain
  appId      AppId
  domainId   DomainId

  subdomain  Subdomain Maybe

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueDomainSubdomain domainId subdomain

  deriving Show Eq

Domain
  owner      UserId
  domain     DomainName

  insertedAt UTCTime
  modifiedAt UTCTime

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
