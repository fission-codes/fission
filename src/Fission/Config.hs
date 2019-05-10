{-# LANGUAGE FlexibleInstances          #-}
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

data Config = Config
  { _logFunc     :: LogFunc
  , _minLogLevel :: Log.MinLogLevel
  , _ipfsPath    :: IpfsPath
  }

makeLenses ''Config

instance HasLogFunc a => Has LogFunc a where
  hasLens = logFuncL

instance Has IpfsPath Config where
  hasLens = ipfsPath

instance Has LogFunc Config where
  hasLens = logFunc

instance Has Log.MinLogLevel Config where
  hasLens = minLogLevel

instance DefConfig Config where
  defConfig = Config
    { _logFunc     = mkLogFunc Log.simple
    , _minLogLevel = Log.MinLogLevel LevelDebug
    , _ipfsPath    = IpfsPath "/usr/local/bin/ipfs"
    }
