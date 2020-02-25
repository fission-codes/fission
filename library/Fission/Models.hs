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

import           Fission.URL

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

--------------------------------------------------------------------------------

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

------------
-- Events --
------------

UpdateUserDataRootEvent
  userId      UserId
  newDataRoot CID

  writer      DID

  insertedAt  UTCTime

  deriving Show Eq

--------------------------------------------------------------------------------

LoosePin
  ownerId    UserId
  cid        CID

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueCidPerUser ownerId cid

  deriving Show Eq

--------------------------------------------------------------------------------

Domain
  ownerId    UserId
  domainName DomainName

  insertedAt UTCTime
  modifiedAt UTCTime

  Primary domainName
  UniqueDomainNamePerOwner domainName ownerId

  deriving Show Eq

--------------------------------------------------------------------------------

App
  ownerId     UserId
  cid         CID

  insertedAt  UTCTime
  modifiedAt  UTCTime

  deriving Show Eq

------------
-- Events --
------------

CreateAppEvent
  ownerId     UserId
  cid         CID

  insertedAt  UTCTime
  deriving Show Eq

DestroyAppEvent
  appId      AppId
  insertedAt UTCTime

  deriving Show Eq

SetAppCIDEvent
  appId  AppId
  newCID CID

  insertedAt UTCTime

  deriving Show Eq

--------------------------------------------------------------------------------

AppDomain
  appId      AppId

  domainName DomainName
  subdomain  Subdomain Maybe

  insertedAt UTCTime

  UniqueSubdomainPerDomain domainName subdomain !force

  deriving Show Eq

------------
-- Events --
------------

AssociateAppDomainEvent
  appId      AppId

  domainName DomainName
  subdomain  Subdomain Maybe

  insertedAt UTCTime

  deriving Show Eq

DissociateAppDomainEvent
  appId      AppId

  domainName DomainName
  subdomain  Subdomain Maybe

  insertedAt UTCTime

  deriving Show Eq
|]

-- FIXME: event log of all updates

instance Arbitrary UserId where
  arbitrary = toSqlKey <$> arbitrary

instance Digestable UserId where
  digest = digest . UTF8.textShow

instance ToSchema UserId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserId")
      |> pure

instance Arbitrary AppId where
  arbitrary = toSqlKey <$> arbitrary

instance ToSchema AppId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "AppId")
      |> pure
