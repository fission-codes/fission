module Fission.Internal.Mock.Effect.Types
  ( RunIO         (..)
  , RunThrow      (..)
  , RunCatch      (..)
  , GetTime       (..)
  , LogMsg        (..)
  , RunLocalIPFS  (..)
  , RunRemoteIPFS (..)
  ) where

import           Control.Monad.Logger
import qualified Network.IPFS.Types   as IPFS
import qualified RIO.ByteString.Lazy  as Lazy

import           Fission.Prelude

data RunIO = RunIO
  deriving (Eq, Show)

data RunThrow = RunThrow
  deriving (Eq, Show)

data RunCatch = RunCatch
  deriving (Eq, Show)

data GetTime = GetTime
  deriving (Eq, Show)

data LogMsg = LogMsg LogLevel LogStr
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

