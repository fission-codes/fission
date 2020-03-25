{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.ClientM () where

import Fission.Prelude
import Servant.Client

instance MonadTime ClientM where
  currentTime = liftIO getCurrentTime
