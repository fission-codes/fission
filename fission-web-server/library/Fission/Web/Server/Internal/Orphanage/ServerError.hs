{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Web.Server.Internal.Orphanage.ServerError () where

import           RIO
import           Servant.Server

instance Display ServerError where
  display = displayShow
