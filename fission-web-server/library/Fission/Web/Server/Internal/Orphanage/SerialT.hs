{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.SerialT () where

import           Servant.API
import           Streamly.Prelude

import           Fission.Prelude
import           Fission.Web.Server.Stream

instance ToSourceIO a (SerialT IO a) where
  toSourceIO = toSourceT
