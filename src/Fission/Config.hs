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
  { _logger      :: LogFunc
  , _minLogLevel :: Log.MinLogLevel
  , _ipfsPath    :: IpfsPath
  }

makeLenses ''Config

instance Has IpfsPath Config where
  hasLens = ipfsPath

instance DefConfig Config where
  defConfig = Config
    { _logger      = mkLogFunc Log.simple
    , _minLogLevel = Log.MinLogLevel LevelDebug
    , _ipfsPath    = IpfsPath "/usr/local/bin/ipfs"
    }
