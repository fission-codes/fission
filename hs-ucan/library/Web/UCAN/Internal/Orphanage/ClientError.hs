{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.UCAN.Internal.Orphanage.ClientError () where

import           RIO

import           Servant.Client.Core

instance Display ClientError where
  display = displayShow
