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
  , _ipfsBin     :: FilePath
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logger

class HasIPFSBin env where
  ipfsBinL :: Lens' env FilePath

instance HasIPFSBin Env where
  ipfsBinL = ipfsBin

base :: Env
base = Env
  { _logger = mkLogFunc Log.simple
  , _minLogLevel = Log.MinLogLevel LevelDebug
  , _ipfsBin = "/usr/local/bin/ipfs"
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
