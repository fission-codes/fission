module Fission.Log
  ( Logger (..)
  , logFunc
  ) where

import RIO

import Control.Lens (makeLenses)

newtype Logger = Logger { _logFunc :: LogFunc}

makeLenses ''Logger
