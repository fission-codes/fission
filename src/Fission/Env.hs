{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Fission.Env where

import Control.Lens (makeLenses)
import RIO

import qualified Fission.Log as Log

data Env = Env
  { _logger      :: LogFunc
  , _minLogLevel :: Log.MinLogLevel
  , _ipfsPath    :: FilePath
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logger

class HasIPFSPath env where
  ipfsPathL :: Lens' env FilePath

instance HasIPFSPath Env where
  ipfsPathL = ipfsPath

base :: Env
base = Env
  { _logger      = mkLogFunc Log.simple
  , _minLogLevel = Log.MinLogLevel LevelDebug
  , _ipfsPath    = "/usr/local/bin/ipfs"
  }

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Preset
  = Test
  | Development
  | Production
  deriving ( Eq
           , Show
           , Read
           )
