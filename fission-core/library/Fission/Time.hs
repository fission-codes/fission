module Fission.Time
  ( doherty
  , dohertyMicroSeconds
  , module Fission.Time.Seconds.Types
  ) where

import           Fission.Prelude

import           Fission.Time.Seconds.Types

-- | The Doherty theshold in seconds
doherty :: NominalDiffTime
doherty = 0.4

-- | The Doherty threshold in µs
dohertyMicroSeconds :: Seconds Micro Natural
dohertyMicroSeconds = 400_000
