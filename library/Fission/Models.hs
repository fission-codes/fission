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

import qualified Fission.App.Types    as App
import qualified Fission.Domain.Types as Domain
import qualified Fission.Domain.Subdomain.Types as Domain

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

LoosePin
  ownerId    UserId
  cid        CID

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueCidPerUser ownerId cid

  deriving Show Eq

App
  ownerId     UserId
  cid         CID

  name        App.Name
  description App.Description

  insertedAt  UTCTime
  modifiedAt  UTCTime

  UniqueAppNamePerOwner name ownerId

  deriving Show Eq

Domain
  ownerId    UserId
  domainName Domain.Name

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueDomainNamePerOwner domainName ownerId

  deriving Show Eq

AppDomain
  appId      AppId
  domainId   DomainId

  subdomain  Domain.Subdomain Maybe

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueSubdomainPerDomain domainId subdomain !force

  deriving Show Eq
|]

instance Arbitrary UserId where
  arbitrary = toSqlKey <$> arbitrary

-- TODO: remove?
instance Digestable UserId where
  digest = digest . UTF8.textShow

instance ToSchema UserId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserID")
      |> pure
