{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Internal.Constraint
  ( MonadRIO
  , Loggable
  ) where

import RIO

type MonadRIO cfg m = (MonadIO m, MonadReader cfg m)
type Loggable cfg   = (HasLogFunc cfg, HasCallStack)
