module Fission.Internal.Mock.Effect.Types
  ( RunIO         (..)
  , RunThrow      (..)
  , RunCatch      (..)
  , GetTime       (..)
  , LogMsg        (..)
  , RunLocalIPFS  (..)
  , RunRemoteIPFS (..)
  ) where

import           Control.Monad.Logger as Logger
import qualified Network.IPFS.Types   as IPFS

import           Fission.Prelude

import qualified RIO.ByteString.Lazy  as Lazy

data RunIO = RunIO
  deriving (Eq, Show)

data RunThrow = RunThrow
  deriving (Eq, Show)

data RunCatch = RunCatch
  deriving (Eq, Show)

data GetTime = GetTime
  deriving (Eq, Show)

data LogMsg = LogMsg Logger.LogLevel LogStr
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

