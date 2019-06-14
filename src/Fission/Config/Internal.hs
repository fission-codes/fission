module Fission.Config.Internal (fromCfg) where

import RIO

import Data.Has

fromCfg :: (MonadReader cfg m, Has a cfg) => m a
fromCfg = view hasLens
