module Fission.Time
  ( doherty
  , dohertyMicroSeconds
  ) where

import Fission.Prelude

-- | The Doherty theshold
doherty :: NominalDiffTime
doherty = 0.4

dohertyMicroSeconds :: Int
dohertyMicroSeconds = 400000
