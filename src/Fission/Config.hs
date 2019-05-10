{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE Trustworthy                #-}

module Fission.Config where

import RIO

import Control.Lens (makeLenses)

import Data.Has
import System.Envy

import qualified Fission.Log as Log

newtype IpfsPath = IpfsPath { unIpfsPath :: FilePath }
  deriving (Show, IsString)

newtype Logger = Logger { _unLogger :: LogFunc }

makeLenses ''Logger

data Config = Config
  { _logger      :: Logger
  , _minLogLevel :: Log.MinLogLevel
  , _ipfsPath    :: IpfsPath
  }

makeLenses ''Config

instance Has IpfsPath Config where
  hasLens = ipfsPath

instance Has Logger Config where
  hasLens = logger

instance HasLogFunc Config where
  logFuncL = logger . unLogger

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance DefConfig Config where
  defConfig = Config
    { _logger      = mkLogger Log.simple
    , _minLogLevel = Log.MinLogLevel LevelDebug
    , _ipfsPath    = IpfsPath "/usr/local/bin/ipfs"
    }

mkLogger :: (CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()) -> Logger
mkLogger = Logger . mkLogFunc
