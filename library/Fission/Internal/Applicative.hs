module Fission.Internal.Applicative (noop) where

import RIO

noop :: Applicative m => m ()
noop = pure ()
