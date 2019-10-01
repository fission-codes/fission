module Fission.Time
  ( doherty
  , dohertyMicroSeconds
  ) where

import RIO
import RIO.Time

-- | The Doherty theshold
doherty :: NominalDiffTime
doherty = 0.4

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000
