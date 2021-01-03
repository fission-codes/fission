{-# OPTIONS_GHC -fno-warn-orphans #-}

-- FIXME move to web-client
module Fission.CLI.Internal.Orphanage.ClientM () where

import           Fission.Prelude
import           Servant.Client

instance MonadTime ClientM where
  currentTime = liftIO getCurrentTime
