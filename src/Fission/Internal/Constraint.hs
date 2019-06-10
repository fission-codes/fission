module Fission.Internal.Constraint (MonadRIO) where

import RIO

type MonadRIO cfg m = (MonadIO m, MonadReader cfg m)
