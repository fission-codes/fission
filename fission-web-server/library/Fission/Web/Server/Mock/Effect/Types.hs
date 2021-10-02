module Fission.Web.Server.Mock.Effect.Types
  ( RunDB              (..)
  , RunAWS             (..)
  , UpdateRoute53      (..)
  , ClearRoute53       (..)
  , SetDNSLink         (..)
  , FollowDNSLink      (..)
  , CreateHerokuAddOn  (..)
  , DestroyHerokuAddOn (..)
  , RetrieveUser       (..)
  , CreateUser         (..)
  , ModifyUser         (..)
  , DestroyUser        (..)
  , RetrieveLoosePin   (..)
  , CreateLoosePin     (..)
  , DestroyLoosePin    (..)
  ) where

import           Data.UUID                   as UUID
import qualified Network.IPFS.Types          as IPFS

import           Fission.Prelude

import qualified Fission.Key                 as Key
import           Fission.URL.Types

import           Fission.User.Email.Types
import           Fission.User.Username.Types

import           Fission.Web.Server.Models

data RunDB = RunDB
  deriving (Eq, Show)

data RunAWS = RunAWS
  deriving (Eq, Show)

data UpdateRoute53 = UpdateRoute53
  deriving (Eq, Show)

data ClearRoute53 = ClearRoute53
  deriving (Eq, Show)

data SetDNSLink = SetDNSLink
  deriving (Eq, Show)

data FollowDNSLink = FollowDNSLink URL (Path URL)
  deriving (Eq, Show)

data DestroyHerokuAddOn = DestroyHerokuAddOn UUID
  deriving (Eq, Show)

data CreateHerokuAddOn = CreateHerokuAddOn UUID
  deriving (Eq, Show)

data RetrieveUser
  = GetUserByUsername      Username
  | GetUserByPublicKey     Key.Public
  | GetUserByHerokuAddOnId HerokuAddOnId
  | GetUserByEmail         Email
  | GetUserById            UserId
  deriving (Eq, Show)

data CreateUser = CreateUser
  deriving (Eq, Show)

data ModifyUser = ModifyUser UserId
  deriving (Eq, Show)

data DestroyUser = DestroyUser UserId
  deriving (Eq, Show)

data RetrieveLoosePin
  = GetLoosePinByUserId UserId
  | GetLoosePinByCID    IPFS.CID
  deriving (Eq, Show)

data CreateLoosePin = CreateLoosePin UserId IPFS.CID
  deriving (Eq, Show)

data DestroyLoosePin
  = DestroyLoosePin     UserId IPFS.CID
  | DestroyLoosePinById UserId LoosePinId
  deriving (Eq, Show)
