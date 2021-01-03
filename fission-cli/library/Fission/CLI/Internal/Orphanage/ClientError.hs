{-# OPTIONS_GHC -fno-warn-orphans #-}

-- FIXME move to web-client
module Fission.CLI.Internal.Orphanage.ClientError () where

import           RIO
import           Servant.Client

instance Display ClientError where
  display = displayShow
