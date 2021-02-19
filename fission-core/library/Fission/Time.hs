module Fission.Time
  ( doherty
  , dohertyMicroSeconds
  ) where

import           Fission.Prelude

-- | The Doherty theshold in seconds
doherty :: NominalDiffTime
doherty = 0.4

-- | The Doherty threshold in µs
dohertyMicroSeconds :: Micro (Seconds Natural)
dohertyMicroSeconds = 400_000
