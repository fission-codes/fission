{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fission.Internal.Constraint
  ( WithRIO
  , Loggable
  ) where

import RIO

type WithRIO env m = (MonadIO m, MonadReader env m)
type Loggable env  = (HasLogFunc env, HasCallStack)
