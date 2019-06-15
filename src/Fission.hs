module Fission
  ( fromConfig
  , simply
  ) where

import RIO

import Data.Has

import qualified Fission.Log as Log

fromConfig :: (MonadReader cfg m, Has a cfg) => m a
fromConfig = view hasLens

simply :: RIO LogFunc a -> IO a
simply = runRIO (mkLogFunc Log.simple)
