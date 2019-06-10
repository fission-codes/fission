{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fission.Config
  ( Config (..)
  , logFunc
  , minLogLevel
  , IPFSPath (..)
  , ipfsPath
  , DBPath (..)
  , dbPath
  , DBPool (..)
  , dbPool
  , Host (..)
  , host
  , base
  , fromCfg
  ) where

import RIO

import Control.Lens (makeLenses)

import Data.Has
import Data.Pool

import Database.Selda.Backend

import qualified Fission.Log as Log

newtype IPFSPath = IPFSPath { getIPFSPath :: FilePath }
  deriving (Show, IsString)

newtype DBPath = DBPath { getDBPath :: FilePath }
  deriving (Show, IsString)

newtype DBPool = DBPool { getPool :: Pool SeldaConnection }

newtype Host = Host { getHost :: Text }
  deriving (Show, IsString)

data Config = Config
  { _logFunc     :: !LogFunc
  , _minLogLevel :: !Log.MinLogLevel
  , _ipfsPath    :: !IPFSPath
  , _host        :: !Host
  , _dbPath      :: !DBPath
  , _dbPool      :: !DBPool
  }

makeLenses ''Config

instance Has IPFSPath Config where
  hasLens = ipfsPath

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance Has DBPath Config where
  hasLens = dbPath

instance Has DBPool Config where
  hasLens = dbPool

instance Has Host Config where
  hasLens = host

instance HasLogFunc Config where
  logFuncL = logFunc

base :: DBPool -> Config
base pool = Config
    { _logFunc     = mkLogFunc Log.simple
    , _minLogLevel = Log.MinLogLevel LevelDebug
    , _ipfsPath    = IPFSPath "/usr/local/bin/ipfs"
    , _host        = Host "localhost:3000"
    , _dbPath      = DBPath "ipfs-api.sqlite"
    , _dbPool      = pool
    }

fromCfg :: (MonadReader cfg m, Has a cfg) => m a
fromCfg = view hasLens
