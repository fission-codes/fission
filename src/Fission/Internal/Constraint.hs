{-# LANGUAGE ConstraintKinds #-}

module Fission.Internal.Constraint
  ( WithRIO
  , Loggable
  ) where

import RIO

type WithRIO m env = (MonadIO m, MonadReader env m)
type Loggable env  = (HasLogFunc env, HasCallStack)
