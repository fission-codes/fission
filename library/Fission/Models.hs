{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# LANGUAGE NoDeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Models where

import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Data.Aeson.Types
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

import qualified Fission.App.Domain.Types as App.Domain

import           Fission.Internal.Orphanage.CID  ()
import           Fission.Internal.Orphanage.UUID ()

import qualified Fission.Internal.UTF8 as UTF8

share
  [ mkPersist       sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate       "migrateAll"
  ] [persistLowerCase|
HerokuAddOn
  uuid       UUID         sqltype=uuid
  region     Region Maybe

  insertedAt UTCTime
  modifiedAt UTCTime

  UniqueUUID uuid

  deriving Show Eq

--------------------------------------------------------------------------------

User
  publicKey     Key.PublicKey Maybe -- Renamed from DID
  algorithm     Key.Algorithm Maybe

  email         Email         Maybe
  username      Username

  role          Role
  active        Bool

  dataRoot      CID

  herokuAddOnId HerokuAddOnId Maybe
  secretDigest  SecretDigest  Maybe

  insertedAt    UTCTime
  modifiedAt    UTCTime

  UniqueUsername  username
  UniquePublicKey publicKey !force
  UniqueEmail     email     !force

  deriving Show Eq

------------
-- Events --
------------

UpdateUserDataRootEvent
  userId      UserId
  newDataRoot CID

  writer      PublicKey

  insertedAt  UTCTime

  deriving Show Eq

--------------------------------------------------------------------------------

LoosePin
  ownerId    UserId
  cid        CID

  insertedAt UTCTime

  UniqueCidPerUser ownerId cid

  deriving Show Eq

--------------------------------------------------------------------------------

Domain
  ownerId    UserId
  domainName DomainName

  insertedAt UTCTime
  modifiedAt UTCTime

  Primary domainName

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
  appId         AppId

  domainName    DomainName

  subdomain     Subdomain Maybe
  isBareDomain  App.Domain.IsBare Maybe -- Hack around nullable constraint

  insertedAt    UTCTime

  UniqueSubdomainPerDomain domainName subdomain    !force
  UniqueBareDomain         domainName isBareDomain !force -- Hack around nullable constraint

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

------------
-- UserId --
------------

instance Arbitrary UserId where
  arbitrary = toSqlKey <$> arbitrary

instance Display UserId where
  display = displayShow

instance Digestable UserId where
  digest = digest . UTF8.textShow

instance ToSchema UserId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "UserId")
      |> pure

-----------
-- AppId --
-----------

instance Arbitrary AppId where
  arbitrary = toSqlKey <$> arbitrary

instance ToSchema AppId where
  declareNamedSchema _ =
    mempty
      |> type_ ?~ SwaggerInteger
      |> NamedSchema (Just "AppId")
      |> pure

instance ToParamSchema AppId where
  toParamSchema _ = mempty |> type_ ?~ SwaggerString

instance Display AppId where
  display = displayShow

instance ToJSONKey AppId where
  toJSONKey = toJSONKeyText textDisplay
