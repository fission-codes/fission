{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.IPFS.Client.Pin.Response () where

import qualified Network.IPFS.Client.Pin as Pin

import           Fission.Prelude

instance Display Pin.Response where
  display response = displayShow response
