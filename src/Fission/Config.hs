{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Config where

import RIO

import Control.Lens (makeLenses)
import System.Envy

import qualified Fission.Log as Log

data Config = Config
  { _logger      :: LogFunc
  , _minLogLevel :: Log.MinLogLevel
  , _ipfsPath    :: FilePath
  }

makeLenses ''Config

instance HasLogFunc Config where
  logFuncL = logger

class HasIPFSPath cfg where
  ipfsPathL :: Lens' cfg FilePath

instance HasIPFSPath Config where
  ipfsPathL = ipfsPath

instance DefConfig Config where
  defConfig = Config
    { _logger      = mkLogFunc Log.simple
    , _minLogLevel = Log.MinLogLevel LevelDebug
    , _ipfsPath    = "/usr/local/bin/ipfs"
    }
