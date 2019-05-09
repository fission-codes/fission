{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Internal.Constraint
  ( WithRIO
  , Loggable
  ) where

import RIO

type WithRIO cfg m = (MonadIO m, MonadReader cfg m)
type Loggable cfg  = (HasLogFunc cfg, HasCallStack)
