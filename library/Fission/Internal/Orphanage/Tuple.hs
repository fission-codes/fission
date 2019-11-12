{-# OPTIONS_GHC -fno-warn-orphans #-}

module Fission.Internal.Orphanage.Tuple () where

import Control.Lens    (_1)
import Fission.Prelude

instance HasLogFunc (LogFunc, b) where
  logFuncL = _1

instance HasLogFunc (LogFunc, b, c) where
  logFuncL = _1
