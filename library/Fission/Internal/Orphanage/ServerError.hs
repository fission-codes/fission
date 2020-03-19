{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ServerError () where

import Servant.Server
import RIO

instance Display ServerError where
  display = displayShow
