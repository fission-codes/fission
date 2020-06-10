module Fission.Internal.Mock.Effect.Types
  ( RunDB              (..)
  , CheckTime          (..)
  , RunAWS             (..)
  , UpdateRoute53      (..)
  , ClearRoute53       (..)
  , SetDNSLink         (..)
  , FollowDNSLink      (..)
  , RunLocalIPFS       (..)
  , RunRemoteIPFS      (..)
  , LogMsg             (..)
  , CreateHerokuAddOn  (..)
  , DestroyHerokuAddOn (..)
  , RetrieveUser       (..)
  , CreateUser         (..)
  , ModifyUser         (..)
  , DestroyUser        (..)
  , RetrieveLoosePin   (..)
  , CreateLoosePin     (..)
  , DestroyLoosePin    (..)
  , FissionEffs
  ) where

import qualified Network.IPFS.Types  as IPFS
import qualified RIO.ByteString.Lazy as Lazy

import           Control.Monad.Logger
import           Data.UUID as UUID

import           Fission.Prelude
import qualified Fission.Key as Key
import           Fission.Models

import           Fission.URL.Types

import           Fission.User.Email.Types
import           Fission.User.Username.Types

type FissionEffs =
  '[ CheckTime
   , RunAWS
   , RunDB
   , SetDNSLink
   , FollowDNSLink
   , UpdateRoute53
   , ClearRoute53
   , RunLocalIPFS
   , RunRemoteIPFS
   , LogMsg
   , DestroyHerokuAddOn
   , DestroyUser
   , DestroyLoosePin
   , RetrieveUser
   , RetrieveLoosePin
   , ModifyUser
   , CreateUser
   , CreateLoosePin
   , CreateHerokuAddOn
   ]

data RunDB
  = RunDB
  deriving (Eq, Show)

data CheckTime
  = CheckTime
  deriving (Eq, Show)

data RunAWS
  = RunAWS
  deriving (Eq, Show)

data UpdateRoute53
  = UpdateRoute53
  deriving (Eq, Show)

data ClearRoute53
  = ClearRoute53
  deriving (Eq, Show)

data SetDNSLink
  = SetDNSLink
  deriving (Eq, Show)

data FollowDNSLink
  = FollowDNSLink URL (Path URL)
  deriving (Eq, Show)

data RunLocalIPFS
  = RunLocalIPFS
  deriving (Eq, Show)

data RunRemoteIPFS
  = RemoteIPFSGeneric
  | RemoteIPFSAdd   Lazy.ByteString
  | RemoteIPFSCat   IPFS.CID
  | RemoteIPFSPin   IPFS.CID
  | RemoteIPFSUnpin IPFS.CID Bool
  deriving (Eq, Show)

data LogMsg
  = LogMsg LogLevel LogStr
  deriving (Eq, Show)

data DestroyHerokuAddOn
  = DestroyHerokuAddOn UUID
  deriving (Eq, Show)

data CreateHerokuAddOn
  = CreateHerokuAddOn UUID
  deriving (Eq, Show)

data RetrieveUser
  = GetUserByUsername      Username
  | GetUserByPublicKey     Key.Public
  | GetUserByHerokuAddOnId HerokuAddOnId
  | GetUserByEmail         Email
  | GetUserById            UserId
  deriving (Eq, Show)

data CreateUser
  = CreateUser
  deriving (Eq, Show)

data ModifyUser
  = ModifyUser UserId
  deriving (Eq, Show)

data DestroyUser
  = DestroyUser UserId
  deriving (Eq, Show)

data RetrieveLoosePin
  = GetLoosePinByUserId UserId
  | GetLoosePinByCID    IPFS.CID
  deriving (Eq, Show)

data CreateLoosePin
  = CreateLoosePin UserId IPFS.CID
  deriving (Eq, Show)

data DestroyLoosePin
  = DestroyLoosePin     UserId IPFS.CID
  | DestroyLoosePinById UserId LoosePinId
  deriving (Eq, Show)
