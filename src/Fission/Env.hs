{-# LANGUAGE TemplateHaskell #-}

module Fission.Env where

import Control.Lens (makeLenses)
import RIO

import qualified Fission.Log as Log

data Env = Env
  { _logger :: LogFunc
  }

makeLenses ''Env

instance HasLogFunc Env where
  logFuncL = logger

base :: Env
base = Env
  { _logger = mkLogFunc Log.simple
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
