{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fission.Config
  ( Config (..)
  , logFunc
  , minLogLevel
  , IpfsPath (..)
  , ipfsPath
  , DBPath (..)
  , dbPath
  , DBPool (..)
  , dbPool
  , Host (..)
  , host
  , base
  ) where

import RIO

import Control.Lens (makeLenses)

import Data.Has
import Data.Pool

import Database.Selda.Backend

import qualified Fission.Log as Log

newtype IpfsPath = IpfsPath { unIpfsPath :: FilePath }
  deriving (Show, IsString)

newtype DBPath = DBPath { unDBPath :: FilePath }
  deriving (Show, IsString)

newtype DBPool = DBPool { unPool :: Pool SeldaConnection }

newtype Host = Host { unHost :: Text }
  deriving (Show, IsString)

data Config = Config
  { _logFunc     :: !LogFunc
  , _minLogLevel :: !Log.MinLogLevel
  , _ipfsPath    :: !IpfsPath
  , _host        :: !Host
  , _dbPath      :: !DBPath
  , _dbPool      :: !DBPool
  }

makeLenses ''Config

instance Has IpfsPath Config where
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
    , _ipfsPath    = IpfsPath "/usr/local/bin/ipfs"
    , _host        = Host "localhost:3000"
    , _dbPath      = DBPath "fission.sqlite"
    , _dbPool      = pool
    }
