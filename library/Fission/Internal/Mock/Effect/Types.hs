module Fission.Internal.Mock.Effect.Types
  ( RunDB          (..)
  , GetLinkedPeers (..)
  , GetVerifier    (..)
  , CheckTime      (..)
  , RunAWS         (..)
  , UpdateRoute53  (..)
  , SetDNSLink     (..)
  , RunLocalIPFS   (..)
  , RunRemoteIPFS  (..)
  , LogMsg         (..)
  ) where

import qualified Network.IPFS.Types  as IPFS
import qualified RIO.ByteString.Lazy as Lazy

import Control.Monad.Logger

import           Fission.Prelude

data RunDB = RunDB
  deriving (Eq, Show)

newtype GetLinkedPeers = GetLinkedPeers (NonEmpty IPFS.Peer)
  deriving (Eq, Show)

data GetVerifier = GetVerifier
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
