{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Fission.Config
  ( Config (..)
  , AuthUsername (..)
  , AuthPassword (..)
  , IpfsPath (..)
  , DBPath (..)
  , logFunc
  , minLogLevel
  , ipfsPath
  , authUsername
  , authPassword
  , dbPath
  ) where

import RIO

import Control.Lens (makeLenses)

import Data.Has
import System.Envy

import qualified Fission.Log as Log

newtype IpfsPath = IpfsPath { unIpfsPath :: FilePath }
  deriving (Show, IsString)

newtype DBPath = DBPath { unDB :: FilePath }
  deriving (Show, IsString)

newtype AuthUsername = AuthUsername { unAuthUsername :: ByteString }
newtype AuthPassword = AuthPassword { unAuthPassword :: ByteString }

data Config = Config
  { _logFunc      :: LogFunc
  , _minLogLevel  :: Log.MinLogLevel
  , _ipfsPath     :: IpfsPath
  , _authUsername :: AuthUsername -- FIXME
  , _authPassword :: AuthPassword -- FIXME
  , _dbPath       :: DBPath
  }

makeLenses ''Config

instance Has IpfsPath Config where
  hasLens = ipfsPath

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance Has AuthUsername Config where
  hasLens = authUsername

instance Has AuthPassword Config where
  hasLens = authPassword

instance Has DBPath Config where
  hasLens = dbPath

instance HasLogFunc Config where
  logFuncL = logFunc

instance DefConfig Config where
  defConfig = Config
    { _logFunc      = mkLogFunc Log.simple
    , _minLogLevel  = Log.MinLogLevel LevelDebug
    , _ipfsPath     = IpfsPath "/usr/local/bin/ipfs"
    , _authUsername = AuthUsername "CHANGEME" -- FIXME
    , _authPassword = AuthPassword "SUPERSECRET" -- FIXME
    , _dbPath       = DBPath "fission.sqlite"
    }
