{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.IPFS.Internal.Orphanage.Utf8Builder () where

import RIO

import Control.Monad.Logger

instance ToLogStr Utf8Builder where
  toLogStr (Utf8Builder builder) = toLogStr builder
