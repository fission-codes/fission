{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

{-# LANGUAGE NoDeriveAnyClass     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Fission.Models where

import           Fission.Error.AlreadyExists.Types

import qualified Crypto.PubKey.RSA                         as RSA

import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Data.Aeson.Types
import           Data.Swagger
import           Data.UUID

import           Network.IPFS.Bytes.Types
import           Network.IPFS.CID.Types

import qualified Fission.Internal.UTF8                     as UTF8
import           Fission.Prelude

import           Fission.Platform.Heroku.Region.Types
import           Fission.Security

import qualified Fission.Key                               as Key
import           Fission.URL

import           Fission.Challenge.Types
import           Fission.User.Email.Types
import           Fission.User.Role.Types
import           Fission.User.Username.Types

import qualified Fission.AWS.Zone.Types                    as AWS

import           Fission.Internal.Orphanage.Bytes          ()
import           Fission.Internal.Orphanage.CID            ()
import           Fission.Internal.Orphanage.RSA2048.Public ()
import           Fission.Internal.Orphanage.UUID           ()

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
  publicKey     Key.Public      Maybe
  exchangeKeys  [RSA.PublicKey] Maybe -- Because Postgresql is being a pain

  email         Email           Maybe
  username      Username

  role          Role
  active        Bool
  verified      Bool                  default=false

  dataRoot      CID
  dataRootSize  Bytes                 default=0

  herokuAddOnId HerokuAddOnId   Maybe
  secretDigest  SecretDigest    Maybe

  insertedAt    UTCTime
  modifiedAt    UTCTime

  UniqueUsername  username
  UniquePublicKey publicKey !force
  -- UniqueEmail     email     !force -- FIXME

  deriving Show Eq

--------------------------------------------------------------------------------

UserChallenge
  userId    UserId
  hash      Challenge

  UniqueUserId    userId
  UniqueChallenge hash

  deriving Show Eq

------------
-- Events --
------------

UpdateUserDataRootEvent
  userId          UserId

  newDataRoot     CID
  newDataRootSize Bytes default=0

  insertedAt      UTCTime

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
  domainName DomainName

  ownerId    UserId
  zoneId     AWS.ZoneID

  insertedAt UTCTime
  modifiedAt UTCTime

  Primary      domainName
  UniqueZoneId zoneId

  deriving Show Eq

--------------------------------------------------------------------------------

App
  ownerId     UserId
  cid         CID

  size        Bytes default=0

  insertedAt  UTCTime
  modifiedAt  UTCTime

  deriving Show Eq

------------
-- Events --
------------

CreateAppEvent
  appId       AppId
  ownerId     UserId

  cid         CID
  size        Bytes default=0

  insertedAt  UTCTime

  deriving Show Eq

DestroyAppEvent
  appId      AppId
  insertedAt UTCTime

  deriving Show Eq

SetAppCIDEvent
  appId      AppId

  newCID     CID
  newCIDSize Bytes default=0

  insertedAt UTCTime

  deriving Show Eq

--------------------------------------------------------------------------------

AppDomain
  appId         AppId

  domainName    DomainName
  subdomain     Subdomain Maybe

  isPrimary     Checkmark nullable

  insertedAt    UTCTime

  UniquePrimaryForApp      appId      isPrimary !force
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

instance Display (AlreadyExists User) where
  display _ = "User already exists"

instance Display (AlreadyExists UserChallenge) where
  display _ = "Challenge already exists"

instance Display (AlreadyExists LoosePin) where
  display _ = "Loose pin already exists"

instance Display (AlreadyExists Domain) where
  display _ = "Domain already exists"

instance Display (AlreadyExists App) where
  display _ = "App already exists"

instance Display (AlreadyExists HerokuAddOn) where
  display _ = "Heroku add-on already exists"

instance Display (NotFound User) where
  display _ = "User not found"

instance Display (NotFound UserChallenge) where
  display _ = "Challenge not found"

instance Display (NotFound LoosePin) where
  display _ = "Loose pin not found"

instance Display (NotFound Domain) where
  display _ = "Domain not found in system"

instance Display (NotFound App) where
  display _ = "App not found"

instance Display (NotFound AppDomain) where
  display _ = "App/Domain relation not found"
