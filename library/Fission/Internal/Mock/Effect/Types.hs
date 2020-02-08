module Fission.Internal.Mock.Effect.Types
  ( RunDB              (..)
  , CheckTime          (..)
  , RunAWS             (..)
  , UpdateRoute53      (..)
  , SetDNSLink         (..)
  , RunLocalIPFS       (..)
  , RunRemoteIPFS      (..)
  , LogMsg             (..)
  , CreateHerokuAddOn  (..)
  , DestroyHerokuAddOn (..)
  , RetrieveUser       (..)
  , CreateUser         (..)
  , ModifyUser         (..)
  , DestroyUser        (..)
  , RetrieveUserCID    (..)
  , CreateUserCID      (..)
  , DestroyUserCID     (..)
  , FissionEffs
  ) where

import qualified Network.IPFS.Types  as IPFS
import qualified RIO.ByteString.Lazy as Lazy

import           Control.Monad.Logger
import           Data.UUID as UUID

import           Fission.Prelude
import           Fission.Models

import           Fission.User.DID.Types
import           Fission.User.Username.Types

type FissionEffs =
  '[ CheckTime
   , RunAWS
   , RunDB
   , SetDNSLink
   , UpdateRoute53
   , RunLocalIPFS
   , RunRemoteIPFS
   , LogMsg
   , DestroyHerokuAddOn
   , DestroyUser
   , DestroyUserCID
   , RetrieveUser
   , RetrieveUserCID
   , ModifyUser
   , CreateUser
   , CreateUserCID
   , CreateHerokuAddOn
   ]

data RunDB = RunDB
  deriving (Eq, Show)

data CheckTime = CheckTime
  deriving (Eq, Show)

data RunAWS = RunAWS
  deriving (Eq, Show)

data UpdateRoute53 = UpdateRoute53
  deriving (Eq, Show)

data SetDNSLink = SetDNSLink
  deriving (Eq, Show)

data RunLocalIPFS = RunLocalIPFS
  deriving (Eq, Show)

data RunRemoteIPFS
  = RemoteIPFSGeneric
  | RemoteIPFSAdd   Lazy.ByteString
  | RemoteIPFSCat   IPFS.CID
  | RemoteIPFSPin   IPFS.CID
  | RemoteIPFSUnpin IPFS.CID Bool
  deriving (Eq, Show)

data LogMsg = LogMsg LogLevel LogStr
  deriving (Eq, Show)

data DestroyHerokuAddOn = DestroyHerokuAddOn UUID
  deriving (Eq, Show)

data CreateHerokuAddOn = CreateHerokuAddOn UUID
  deriving (Eq, Show)

data RetrieveUser
  = GetUserByUsername Username
  | GetUserByDid      DID
  | GetUserByHerokuAddOnId HerokuAddOnId
  deriving (Eq, Show)

data CreateUser = CreateUser
  deriving (Eq, Show)

data ModifyUser = ModifyUser UserId
  deriving (Eq, Show)

data DestroyUser = DestroyUser UserId
  deriving (Eq, Show)

data RetrieveUserCID
  = GetUserCIDByUserId UserId
  | GetUserCIDByCID    IPFS.CID
  deriving (Eq, Show)

data CreateUserCID = CreateUserCID UserId IPFS.CID
  deriving (Eq, Show)

data DestroyUserCID
  = DestroyUserCID UserId IPFS.CID
  | DestroyUserCIDById UserCidId
  deriving (Eq, Show)
