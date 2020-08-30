{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ClientError () where

import           RIO
import           Servant.Client

instance Display ClientError where
  display = displayShow
